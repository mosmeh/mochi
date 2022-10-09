pub const MAX_UTF8: u32 = 0x7fffffff;

pub fn is_utf8_continuation_byte(b: u8) -> bool {
    b & 0xc0 == 0x80
}

pub fn encode_utf8(mut ch: u32, buf: &mut Vec<u8>) -> bool {
    if ch > MAX_UTF8 {
        return false;
    }
    if ch < 0x80 {
        buf.push(ch as u8);
        return true;
    }

    const MAX_ENCODED_LEN: usize = 6;
    let mut encoded = [0; MAX_ENCODED_LEN];
    let mut len = 1;
    let mut mfb = 0x3f;
    loop {
        encoded[MAX_ENCODED_LEN - len] = 0x80 | (ch as u8 & 0x3f);
        len += 1;
        ch >>= 6;
        mfb >>= 1;
        if ch <= mfb as u32 {
            break;
        }
    }
    encoded[MAX_ENCODED_LEN - len] = (!mfb << 1) | ch as u8;
    buf.extend_from_slice(&encoded[MAX_ENCODED_LEN - len..]);

    true
}

pub fn decode_utf8<B: AsRef<[u8]>>(bytes: B) -> Option<(u32, usize)> {
    let bytes = bytes.as_ref();
    let mut ch = match bytes.first() {
        None => return None,
        Some(&ch) if ch < 0x80 => return Some((ch as u32, 1)),
        Some(&ch) => ch as u32,
    };

    const LIMITS: [u32; 6] = [!0, 0x80, 0x800, 0x10000, 0x200000, 0x4000000];
    let mut count = 0;
    let mut res = 0;
    while ch & 0x40 != 0 {
        count += 1;
        let cc = *bytes.get(count)?;
        if !is_utf8_continuation_byte(cc) {
            return None;
        }
        res = (res << 6) | ((cc & 0x3f) as u32);
        ch <<= 1;
    }
    res |= (ch & 0x7f) << (count * 5);
    (res <= MAX_UTF8 && res >= LIMITS[count]).then_some((res, count + 1))
}

pub fn trim_whitespaces(bytes: &[u8]) -> &[u8] {
    fn is_whitespace(ch: u8) -> bool {
        // u8::is_ascii_whitespace + 0xb
        matches!(ch, b'\t' | b'\n' | 0xc | b'\r' | b' ' | 0xb)
    }

    let mut slice = bytes;
    while let [first, rest @ ..] = slice {
        if is_whitespace(*first) {
            slice = rest;
        } else {
            break;
        }
    }
    while let [rest @ .., last] = slice {
        if is_whitespace(*last) {
            slice = rest;
        } else {
            break;
        }
    }
    slice
}
