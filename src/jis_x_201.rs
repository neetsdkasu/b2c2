pub fn contains(ch: char) -> bool {
    ch.is_ascii() || matches!(ch, '\u{FF61}'..='\u{FF9F}' | '\u{FFE3}' | '\u{FFE5}')
}

// UTF-8 -> JIS X 201
pub fn convert_from_str(s: &str) -> Vec<u8> {
    s.chars()
        .map(|ch| {
            if ch.is_ascii() {
                ch as u8
            } else {
                match ch as u32 {
                    code @ 0xFF61_u32..=0xFF9F_u32 => (code - 0xFF61_u32) as u8 + 0xA1_u8,
                    0xFFE3_u32 => 0x7E_u8,
                    0xFFE5_u32 => 0x5C_u8,
                    _ => 0xFD_u8,
                }
            }
        })
        .collect()
}

// JIS X 201 -> UTF-8
pub fn convert_to_string(v: &[u8]) -> String {
    use std::convert::TryFrom;
    v.iter()
        .map(|code| {
            if code.is_ascii() {
                *code as char
            } else if let 0xA1_u8..=0xDF_u8 = code {
                char::try_from((code - 0xA1_u8) as u32 + 0xFF61_u32).unwrap()
            } else {
                '\u{FFFD}'
            }
        })
        .collect()
}
