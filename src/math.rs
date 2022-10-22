// ported from musl

pub fn frexp(x: f64) -> (f64, i32) {
    let bits = x.to_bits();
    let exp_bits = (bits >> 52 & 0x7ff) as i32;
    if exp_bits == 0 {
        if x != 0.0 {
            let scale = f64::from_bits(0x43f0000000000000); // 0x1p64
            let (fr, exp) = frexp(x * scale);
            (fr, exp - 64)
        } else {
            (x, 0)
        }
    } else if exp_bits == 0x7ff {
        (x, 0)
    } else {
        (
            f64::from_bits((bits & 0x800fffffffffffff) | 0x3fe0000000000000),
            exp_bits - 0x3fe,
        )
    }
}

pub fn ldexp(mut x: f64, mut n: i32) -> f64 {
    if n > 1023 {
        let scale = f64::from_bits(0x7fe0000000000000); // 0x1p1023
        x *= scale;
        n -= 1023;
        if n > 1023 {
            x *= scale;
            n -= 1023;
            if n > 1023 {
                n = 1023;
            }
        }
    } else if n < -1022 {
        let scale = f64::from_bits(0x10000000000000) * f64::from_bits(0x4340000000000000); // 0x1p-1022 * 0x1p53
        x *= scale;
        n += 1022 - 53;
        if n < -1022 {
            x *= scale;
            n += 1022 - 53;
            if n < -1022 {
                n = -1022;
            }
        }
    }
    x * f64::from_bits(((0x3ff + n) as u64) << 52)
}
