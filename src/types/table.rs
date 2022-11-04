mod bucket;

use super::{Integer, LuaString, NativeClosure, NativeFunction, Number, Value};
use crate::{
    gc::{GarbageCollect, GcCell, GcLifetime, Tracer},
    number_is_valid_integer,
};
use bucket::Bucket;
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, thiserror::Error)]
pub enum TableError {
    #[error("table index is nil")]
    IndexIsNil,

    #[error("table index is NaN")]
    IndexIsNaN,

    #[error("invalid key to 'next'")]
    InvalidKeyToNext,
}

#[derive(Clone, Default)]
pub struct Table<'gc> {
    array: Vec<Value<'gc>>,

    buckets: Vec<Bucket<'gc>>,
    last_free_bucket: usize,

    metatable: Option<GcCell<'gc, Table<'gc>>>,
}

impl std::fmt::Debug for Table<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Table")
            .field("array", &self.array)
            .field("#buckets", &self.buckets.len())
            .field("last_free_bucket", &self.last_free_bucket)
            .field("metatable", &self.metatable)
            .finish()
    }
}

impl<'gc> From<Vec<Value<'gc>>> for Table<'gc> {
    fn from(array: Vec<Value<'gc>>) -> Self {
        Self {
            array,
            ..Default::default()
        }
    }
}

unsafe impl GarbageCollect for Table<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.array.trace(tracer);
        self.buckets.trace(tracer);
        self.metatable.trace(tracer);
    }
}

unsafe impl<'a> GcLifetime<'a> for Table<'_> {
    type Aged = Table<'a>;
}

impl<'gc> Table<'gc> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_size(array_len: usize, hashtable_capacity: usize) -> Self {
        let mut table = Self::default();
        table.resize(array_len, hashtable_capacity);
        table
    }

    pub fn array(&self) -> &[Value<'gc>] {
        &self.array
    }

    pub fn resize_array(&mut self, new_len: usize) {
        self.resize(new_len, self.buckets.len());
    }

    pub fn get<K>(&self, key: K) -> Value<'gc>
    where
        K: Into<Value<'gc>>,
    {
        let mut key = key.into();
        match key {
            Value::Number(x) if number_is_valid_integer(x) => key = Value::Integer(x as Integer),
            _ => (),
        }
        if let Value::Integer(i) = key {
            if let Some(value) = self.array.get((i as usize).wrapping_sub(1)) {
                return *value;
            }
        }
        self.find_bucket(key)
            .map(|index| unsafe { self.buckets.get_unchecked(index) }.value())
            .unwrap_or_default()
    }

    pub fn get_integer_key(&self, i: Integer) -> Value<'gc> {
        if let Some(value) = self.array.get((i as usize).wrapping_sub(1)) {
            return *value;
        }
        self.find_integer_key_bucket(i)
            .map(|index| unsafe { self.buckets.get_unchecked(index) }.value())
            .unwrap_or_default()
    }

    pub fn get_field(&self, field: LuaString<'gc>) -> Value<'gc> {
        self.find_string_key_bucket(field)
            .map(|index| unsafe { self.buckets.get_unchecked(index) }.value())
            .unwrap_or_default()
    }

    pub fn set<K, V>(&mut self, key: K, value: V) -> Result<(), TableError>
    where
        K: Into<Value<'gc>>,
        V: Into<Value<'gc>>,
    {
        let mut key = key.into();
        let value = value.into();
        match key {
            Value::Nil => return Err(TableError::IndexIsNil),
            Value::Number(x) if x.is_nan() => return Err(TableError::IndexIsNaN),
            Value::Number(x) if number_is_valid_integer(x) => key = Value::Integer(x as Integer),
            _ => (),
        }
        if let Value::Integer(i) = key {
            if let Some(slot) = self.array.get_mut((i as usize).wrapping_sub(1)) {
                *slot = value;
                return Ok(());
            }
        }
        if let Some(index) = self.find_bucket(key) {
            unsafe { self.buckets.get_unchecked_mut(index) }.update_or_remove_item(value);
            return Ok(());
        }
        if !value.is_nil() {
            unsafe { self.set_new_hashtable_key(key, value) }
        }
        Ok(())
    }

    pub fn set_integer_key<V>(&mut self, i: Integer, value: V)
    where
        V: Into<Value<'gc>>,
    {
        let value = value.into();
        if let Some(slot) = self.array.get_mut((i as usize).wrapping_sub(1)) {
            *slot = value;
            return;
        }
        if let Some(index) = self.find_integer_key_bucket(i) {
            unsafe { self.buckets.get_unchecked_mut(index) }.update_or_remove_item(value);
            return;
        }
        if !value.is_nil() {
            unsafe { self.set_new_hashtable_key(Value::Integer(i), value) }
        }
    }

    pub fn set_field<V>(&mut self, field: LuaString<'gc>, value: V)
    where
        V: Into<Value<'gc>>,
    {
        let value = value.into();
        if let Some(index) = self.find_string_key_bucket(field) {
            unsafe { self.buckets.get_unchecked_mut(index) }.update_or_remove_item(value);
            return;
        }
        if !value.is_nil() {
            unsafe { self.set_new_hashtable_key(Value::String(field), value) }
        }
    }

    pub(crate) fn replace<K, V>(&mut self, key: K, value: V) -> Result<bool, TableError>
    where
        K: Into<Value<'gc>>,
        V: Into<Value<'gc>>,
    {
        let mut key = key.into();
        match key {
            Value::Nil => return Err(TableError::IndexIsNil),
            Value::Number(x) if x.is_nan() => return Err(TableError::IndexIsNaN),
            Value::Number(x) if number_is_valid_integer(x) => key = Value::Integer(x as Integer),
            _ => (),
        }
        if let Value::Integer(i) = key {
            match self.array.get_mut((i as usize).wrapping_sub(1)) {
                Some(Value::Nil) => return Ok(false),
                Some(slot) => {
                    *slot = value.into();
                    return Ok(true);
                }
                None => (),
            }
        }
        if let Some(index) = self.find_bucket(key) {
            let bucket = unsafe { self.buckets.get_unchecked_mut(index) };
            if bucket.has_value() {
                bucket.update_or_remove_item(value.into());
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(crate) fn replace_integer_key<V>(&mut self, i: Integer, value: V) -> bool
    where
        V: Into<Value<'gc>>,
    {
        match self.array.get_mut((i as usize).wrapping_sub(1)) {
            Some(Value::Nil) => return false,
            Some(slot) => {
                *slot = value.into();
                return true;
            }
            None => (),
        }
        if let Some(index) = self.find_integer_key_bucket(i) {
            let bucket = unsafe { self.buckets.get_unchecked_mut(index) };
            if bucket.has_value() {
                bucket.update_or_remove_item(value.into());
                return true;
            }
        }
        false
    }

    pub(crate) fn replace_field<V>(&mut self, field: LuaString<'gc>, value: V) -> bool
    where
        V: Into<Value<'gc>>,
    {
        if let Some(index) = self.find_string_key_bucket(field) {
            let bucket = unsafe { self.buckets.get_unchecked_mut(index) };
            if bucket.has_value() {
                bucket.update_or_remove_item(value.into());
                return true;
            }
        }
        false
    }

    pub fn metatable(&self) -> Option<GcCell<'gc, Table<'gc>>> {
        self.metatable
    }

    pub fn set_metatable<T>(&mut self, metatable: T)
    where
        T: Into<Option<GcCell<'gc, Table<'gc>>>>,
    {
        self.metatable = metatable.into();
    }

    pub fn lua_len(&self) -> Integer {
        if let Some(Value::Nil) = self.array.last() {
            let mut i = 0;
            let mut j = self.array.len();
            while j - i > 1 {
                let m = (j - i) / 2 + i;
                if !self.array[m - 1].is_nil() {
                    i = m;
                } else {
                    j = m;
                }
            }
            return i as Integer;
        }
        if self.buckets.is_empty() {
            return self.array.len() as Integer;
        }

        // exponential search
        let mut i = self.array.len() as Integer;
        let mut j = i + 1;
        while self
            .find_bucket(j.into())
            .map(|index| self.buckets[index].has_value())
            .unwrap_or_default()
        {
            if j == Integer::MAX {
                return j;
            }
            j = j.saturating_mul(2);
        }
        while j - i > 1 {
            let m = (j - i) / 2 + i;
            if self
                .find_bucket(m.into())
                .map(|index| self.buckets[index].has_value())
                .unwrap_or_default()
            {
                i = m;
            } else {
                j = m;
            }
        }
        i
    }

    pub fn next(&self, key: Value<'gc>) -> Result<Option<(Value<'gc>, Value<'gc>)>, TableError> {
        let next_array_index = match key {
            Value::Nil => Some(0),
            Value::Integer(i) if 1 <= i && i as usize <= self.array.len() => Some(i as usize),
            _ => None,
        };
        let next_bucket_index = if let Some(start) = next_array_index {
            if let Some(index) = self.array[start..].iter().position(|value| !value.is_nil()) {
                let index = start + index;
                let key = (index + 1) as Integer;
                return Ok(Some((key.into(), self.array[index])));
            }
            0
        } else if let Some(index) = self.find_bucket(key) {
            index + 1
        } else {
            return Err(TableError::InvalidKeyToNext);
        };
        if let Some(bucket) = self.buckets[next_bucket_index..]
            .iter()
            .find(|bucket| bucket.has_value())
        {
            debug_assert!(bucket.has_key());
            return Ok(Some((bucket.key(), bucket.value())));
        }
        Ok(None)
    }

    unsafe fn set_new_hashtable_key(&mut self, key: Value<'gc>, value: Value<'gc>) {
        if self.buckets.is_empty() {
            self.rehash(key);
            self.set(key, value).unwrap();
            return;
        }

        let main_index = self.calc_main_bucket_index(key);
        let index = if self.buckets.get_unchecked(main_index).has_value() {
            let free_index = if let Some(free_index) = self.find_free_bucket() {
                free_index
            } else {
                self.rehash(key);
                self.set(key, value).unwrap();
                return;
            };

            let main_bucket = self.buckets.get_unchecked(main_index);
            let main_index_of_colliding_item = self.calc_main_bucket_index(main_bucket.key());

            if main_index_of_colliding_item == main_index {
                if let Some(next_index) = main_bucket.next_index() {
                    self.buckets
                        .get_unchecked_mut(free_index)
                        .set_next_index(next_index);
                } else {
                    debug_assert!(!self.buckets[free_index].has_next());
                }

                self.buckets
                    .get_unchecked_mut(main_index)
                    .set_next_index(free_index);
                free_index
            } else {
                let mut prev_in_chain = main_index_of_colliding_item;
                loop {
                    let next_index = self
                        .buckets
                        .get_unchecked(prev_in_chain)
                        .next_index()
                        .unwrap();
                    if next_index == main_index {
                        break;
                    }
                    prev_in_chain = next_index;
                }
                self.buckets
                    .get_unchecked_mut(prev_in_chain)
                    .set_next_index(free_index);

                let colliding_item = self.buckets.get_unchecked(main_index).clone();
                let free_bucket = self.buckets.get_unchecked_mut(free_index);
                *free_bucket = colliding_item;
                if free_bucket.has_next() {
                    self.buckets
                        .get_unchecked_mut(main_index)
                        .set_next_index(None);
                }

                main_index
            }
        } else {
            main_index
        };

        self.buckets
            .get_unchecked_mut(index)
            .set_new_item(key, value);
    }

    fn find_bucket(&self, key: Value<'gc>) -> Option<usize> {
        if self.buckets.is_empty() {
            return None;
        }

        let mut index = self.calc_main_bucket_index(key);
        loop {
            let bucket = unsafe { self.buckets.get_unchecked(index) };
            if bucket.matches(key) {
                return Some(index);
            }
            if let Some(next_index) = bucket.next_index() {
                index = next_index;
            } else {
                return None;
            }
        }
    }

    fn find_integer_key_bucket(&self, key: Integer) -> Option<usize> {
        if self.buckets.is_empty() {
            return None;
        }

        let mut index = self.calc_main_bucket_index(Value::Integer(key));
        loop {
            let bucket = unsafe { self.buckets.get_unchecked(index) };
            if bucket.matches_integer(key) {
                return Some(index);
            }
            if let Some(next_index) = bucket.next_index() {
                index = next_index;
            } else {
                return None;
            }
        }
    }

    fn find_string_key_bucket(&self, key: LuaString<'gc>) -> Option<usize> {
        if self.buckets.is_empty() {
            return None;
        }

        let mut index = self.calc_main_bucket_index(Value::String(key));
        loop {
            let bucket = unsafe { self.buckets.get_unchecked(index) };
            if bucket.matches_string(key) {
                return Some(index);
            }
            if let Some(next_index) = bucket.next_index() {
                index = next_index;
            } else {
                return None;
            }
        }
    }

    fn calc_main_bucket_index(&self, key: Value) -> usize {
        debug_assert!(!self.buckets.is_empty());
        debug_assert!(self.buckets.len().is_power_of_two());

        let mut state = FxHasher::default();
        key.hash(&mut state);
        (state.finish() as usize) & (self.buckets.len() - 1)
    }

    unsafe fn find_free_bucket(&mut self) -> Option<usize> {
        while self.last_free_bucket > 0 {
            self.last_free_bucket -= 1;
            if !self.buckets.get_unchecked(self.last_free_bucket).has_key() {
                return Some(self.last_free_bucket);
            }
        }
        None
    }

    fn rehash(&mut self, new_key: Value) {
        // create histogram of non negative integer keys

        const NUM_BINS: usize = usize::BITS as usize;
        let mut bins = [0; NUM_BINS];
        let mut num_positive = 0;

        // array part
        match self.array.get(0) {
            Some(Value::Nil) => (),
            Some(_) => {
                bins[0] = 1;
                num_positive += 1;
            }
            _ => (),
        }
        let mut threshold = 1;
        for bin in bins[1..].iter_mut() {
            if threshold >= self.array.len() {
                break;
            }
            let next_threshold = threshold.saturating_mul(2).min(self.array.len());
            for x in &self.array[threshold..next_threshold] {
                if !x.is_nil() {
                    *bin += 1;
                    num_positive += 1;
                }
            }
            threshold = next_threshold;
        }

        let mut num_keys = num_positive;

        // map part
        fn increment_bin(bins: &mut [usize; NUM_BINS], num_positive: &mut usize, i: Integer) {
            if i >= 1 {
                let log2 = Integer::BITS - (i - 1).leading_zeros();
                bins[log2 as usize] += 1;
                *num_positive += 1;
            }
        }

        for bucket in &self.buckets {
            if !bucket.has_value() {
                continue;
            }
            num_keys += 1;
            if let Value::Integer(integer) = bucket.key() {
                increment_bin(&mut bins, &mut num_positive, integer);
            }
        }

        num_keys += 1;
        if let Value::Integer(i) = new_key {
            increment_bin(&mut bins, &mut num_positive, i);
        }

        // determine optimal length of array
        let mut optimal_array_len = 0;
        let mut num_items_in_array = 0;

        let mut bins_cumulative_sum = 0;
        let mut power_of_two = 1;

        for bin in &bins {
            bins_cumulative_sum += *bin;
            if bins_cumulative_sum > power_of_two / 2 {
                optimal_array_len = power_of_two;
                num_items_in_array = bins_cumulative_sum;
            }
            if num_positive <= power_of_two {
                break;
            }
            power_of_two *= 2;
        }

        self.resize(optimal_array_len, num_keys - num_items_in_array);
    }

    fn resize(&mut self, new_array_len: usize, new_num_buckets: usize) {
        let new_num_buckets = if new_num_buckets > 0 {
            new_num_buckets.next_power_of_two()
        } else {
            0
        };
        let old_buckets: Vec<_> = self
            .buckets
            .drain(..)
            .filter(|bucket| bucket.has_value())
            .collect();
        self.buckets.resize(new_num_buckets, Default::default());
        self.last_free_bucket = new_num_buckets;

        if new_array_len < self.array.len() {
            let excess_items = self.array.split_off(new_array_len);
            for (i, x) in excess_items.into_iter().enumerate() {
                self.set((new_array_len + 1 + i) as Integer, x).unwrap();
            }
        } else {
            self.array.resize(new_array_len, Default::default());
        }

        for bucket in old_buckets {
            debug_assert!(bucket.has_key());
            let key = bucket.key();
            let value = bucket.value();
            if let Value::Integer(i @ 1..) = key {
                if let Some(slot) = self.array.get_mut((i - 1) as usize) {
                    *slot = value;
                    continue;
                }
            }
            self.set(key, value).unwrap();
        }
    }
}
