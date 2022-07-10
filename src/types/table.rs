use super::{Integer, Value};
use crate::gc::{Trace, Tracer};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Default)]
pub struct Table<'a> {
    map: FxHashMap<Value<'a>, Value<'a>>,
    array: Vec<Value<'a>>,
}

impl<'a> From<Vec<Value<'a>>> for Table<'a> {
    fn from(array: Vec<Value<'a>>) -> Self {
        Self {
            map: Default::default(),
            array,
        }
    }
}

unsafe impl Trace for Table<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.map.trace(tracer);
        self.array.trace(tracer);
    }
}

impl<'a> Table<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get<K>(&self, key: K) -> Value<'a>
    where
        K: Into<Value<'a>>,
    {
        let key = key.into();
        let key = if let Some(i) = key.as_integer() {
            if i >= 1 {
                if let Some(value) = self.array.get((i - 1) as usize) {
                    return *value;
                }
            }
            Value::Integer(i)
        } else {
            key
        };
        self.map.get(&key).copied().unwrap_or_default()
    }

    pub fn set<K, V>(&mut self, key: K, value: V)
    where
        K: Into<Value<'a>>,
        V: Into<Value<'a>>,
    {
        let key = key.into();
        let value = value.into();

        let key = if let Some(i) = key.as_integer() {
            if i >= 1 {
                if let Some(slot) = self.array.get_mut((i - 1) as usize) {
                    *slot = value;
                    return;
                }
            }
            Value::Integer(i)
        } else {
            key
        };

        if value == Value::Nil {
            self.map.remove(&key);
            return;
        }
        if self.map.len() < self.map.capacity() {
            self.map.insert(key, value);
            return;
        }

        self.resize_array(key);

        if let Value::Integer(i) = key {
            if i >= 1 {
                if let Some(slot) = self.array.get_mut((i - 1) as usize) {
                    *slot = value;
                    return;
                }
            }
        }
        self.map.insert(key, value);
    }

    fn resize_array(&mut self, new_key: Value<'a>) {
        // create histogram of non negative integer keys

        const NUM_BINS: usize = usize::BITS as usize;
        let mut bins = [0; NUM_BINS];
        let mut num_non_neg = 0;

        // array part
        if let Some(x) = self.array.get(0) {
            if *x != Value::Nil {
                bins[0] = 1;
                num_non_neg += 1;
            }
        }
        let mut threshold = 1;
        for bin in bins[1..].iter_mut() {
            if threshold >= self.array.len() {
                break;
            }
            let next_threshold = threshold.saturating_mul(2).min(self.array.len());
            for x in &self.array[threshold..next_threshold] {
                if *x != Value::Nil {
                    *bin += 1;
                    num_non_neg += 1;
                }
            }
            threshold = next_threshold;
        }

        // map part
        fn increment_bin(bins: &mut [usize; NUM_BINS], num_non_neg: &mut usize, key: Value) {
            if let Value::Integer(i) = key {
                if i >= 1 {
                    let log2 = Integer::BITS - (i - 1).leading_zeros();
                    bins[log2 as usize] += 1;
                    *num_non_neg += 1;
                }
            }
        }
        for x in self.map.keys() {
            increment_bin(&mut bins, &mut num_non_neg, *x);
        }
        increment_bin(&mut bins, &mut num_non_neg, new_key);

        // determine optimal length of array
        let mut optimal_len = 0;
        let mut num_elems_in_array = 0;
        let mut power_of_two = 1;
        for bin in &bins {
            num_elems_in_array += *bin;
            if num_elems_in_array > power_of_two / 2 {
                optimal_len = power_of_two;
            }
            if num_non_neg <= power_of_two {
                break;
            }
            power_of_two *= 2;
        }

        // move some elements in map to array
        let old_array_len = self.array.len();
        if optimal_len > old_array_len {
            self.array.resize(optimal_len, Value::Nil);
            self.map.retain(|k, v| {
                if let Value::Integer(i) = *k {
                    if i >= 1 {
                        if let Some(slot) = self.array.get_mut((i - 1) as usize) {
                            *slot = *v;
                            return false;
                        }
                    }
                }
                true
            });
        } else {
            self.map.reserve(self.map.len());
        }
    }

    pub fn lua_len(&self) -> Integer {
        if let Some(Value::Nil) = self.array.last() {
            let mut i = 0;
            let mut j = self.array.len();
            while j - i > 1 {
                let m = (j - i) / 2 + i;
                if self.array[m - 1] != Value::Nil {
                    i = m;
                } else {
                    j = m;
                }
            }
            return i as Integer;
        }
        if self.map.is_empty() {
            return self.array.len() as Integer;
        }

        // exponential search
        let mut i = self.array.len() as Integer;
        let mut j = i + 1;
        while self.map.contains_key(&j.into()) {
            if j == Integer::MAX {
                return j;
            }
            j = j.saturating_mul(2);
        }
        while j - i > 1 {
            let m = (j - i) / 2 + i;
            if self.map.contains_key(&m.into()) {
                i = m;
            } else {
                j = m;
            }
        }
        i
    }
}
