// UTF-8 -> JIS X 201
pub fn convert_from_char(ch: char) -> u8 {
    if ch.is_ascii() {
        ch as u8
    } else {
        match ch as u32 {
            code @ 0xFF61_u32..=0xFF9F_u32 => (code - 0xFF61_u32) as u8 + 0xA1_u8,
            0x00AF_u32 | 0x203E_u32 => 0x7E_u8, // OVER_LINE (replace TILDE)
            _ => ch as u8,
        }
    }
}

// JIS X 201 -> UTF-8
pub fn convert_to_char(code: u8, replace_specials: bool) -> char {
    use std::convert::TryFrom;
    if code.is_ascii() {
        if code == 0x7E_u8 {
            // OVER_LINE (replace from TILDE)
            '\u{203E}'
        } else if (0x20_u8..0x7E_u8).contains(&code) {
            code as char
        } else if replace_specials {
            '\u{FFFD}'
        } else {
            code as char
        }
    } else if let 0xA1_u8..=0xDF_u8 = code {
        char::try_from((code - 0xA1_u8) as u32 + 0xFF61_u32).unwrap_or('\u{FFFD}')
    } else if replace_specials {
        '\u{FFFD}'
    } else {
        code as char
    }
}

const HIRAGANA: &str = "あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをんぁぃぅぇぉゃゅょっ";
const KATAKANA: &str = "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲンァィゥェォャュョッ";
const KANA_HALF_WIDE: &str = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝｧｨｩｪｫｬｭｮｯ";
const ASCII: &str = "　！”＃＄％＆’（）＊＋，－．／０１２３４５６７８９：；＜＝＞？＠ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ［￥］＾＿｀ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ｛｜｝￣";
const KIGOU: &str = "。「」、・ー゛゜";
const KIGOU_HALF_WIDE: &str = "｡｢｣､･ｰﾞﾟ";
const HIRAGANA_DAKUON: &str = "がぎぐげござじずぜぞだぢづでどばびぶべぼぱぴぷぺぽ";
const KATAKANA_DAKUON: &str = "ガギグゲゴザジズゼゾダヂヅデドバビブベボパピプペポ";
const KANA_DAKUON_HALF_WIDE: &str = "ｶﾞｷﾞｸﾞｹﾞｺﾞｻﾞｼﾞｽﾞｾﾞｿﾞﾀﾞﾁﾞﾂﾞﾃﾞﾄﾞﾊﾞﾋﾞﾌﾞﾍﾞﾎﾞﾊﾟﾋﾟﾌﾟﾍﾟﾎﾟ";

// 一部の全角文字を半角文字に変換する (JIS X 201と関係ないコードだね…)
pub fn convert_kana_wide_full_to_half(s: &str) -> String {
    let mut ret = String::new();
    let hiragana: Vec<_> = HIRAGANA.chars().collect();
    let katakana: Vec<_> = KATAKANA.chars().collect();
    let kana_half_wide: Vec<_> = KANA_HALF_WIDE.chars().collect();
    let ascii: Vec<_> = ASCII.chars().collect();
    let kigou: Vec<_> = KIGOU.chars().collect();
    let kigou_half_wide: Vec<_> = KIGOU_HALF_WIDE.chars().collect();
    let hiragana_dakuon: Vec<_> = HIRAGANA_DAKUON.chars().collect();
    let katakana_dakuon: Vec<_> = KATAKANA_DAKUON.chars().collect();
    let kana_dakuon_half_wide: Vec<_> = KANA_DAKUON_HALF_WIDE.chars().collect();
    assert_eq!(hiragana.len(), kana_half_wide.len());
    assert_eq!(katakana.len(), kana_half_wide.len());
    assert_eq!(ascii.len(), 0x7E - 0x20 + 1);
    assert_eq!(kigou.len(), kigou_half_wide.len());
    assert_eq!(hiragana_dakuon.len() * 2, kana_dakuon_half_wide.len());
    assert_eq!(katakana_dakuon.len() * 2, kana_dakuon_half_wide.len());
    for ch in s.chars() {
        if ch.is_ascii() {
            ret.push(ch);
        } else if let Some(index) = hiragana.iter().position(|&x| x == ch) {
            ret.push(*kana_half_wide.get(index).unwrap_or(&ch));
        } else if let Some(index) = katakana.iter().position(|&x| x == ch) {
            ret.push(*kana_half_wide.get(index).unwrap_or(&ch));
        } else if let Some(index) = hiragana_dakuon.iter().position(|&x| x == ch) {
            ret.push(*kana_dakuon_half_wide.get(index * 2).unwrap_or(&ch));
            ret.push(*kana_dakuon_half_wide.get(index * 2 + 1).unwrap_or(&ch));
        } else if let Some(index) = katakana_dakuon.iter().position(|&x| x == ch) {
            ret.push(*kana_dakuon_half_wide.get(index * 2).unwrap_or(&ch));
            ret.push(*kana_dakuon_half_wide.get(index * 2 + 1).unwrap_or(&ch));
        } else if let Some(index) = ascii.iter().position(|&x| x == ch) {
            ret.push((b' ' + index as u8) as char);
        } else if let Some(index) = kigou.iter().position(|&x| x == ch) {
            ret.push(*kigou_half_wide.get(index).unwrap_or(&ch));
        } else if ch == 'ヴ' {
            ret.push_str("ｳﾞ")
        } else if ch == 'ヺ' {
            ret.push_str("ｦﾞ")
        } else {
            ret.push(ch);
        }
    }
    ret
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_works() {
        let s = "あんたがフルフルバーだよん！？~`＃＿ＨＯｈｏ｝ヴヺ＜１２３￥＠＊＞";
        let t = convert_kana_wide_full_to_half(s);
        eprintln!("{}", s);
        eprintln!("{}", t);
        let mut u = String::new();
        for ch in t.chars() {
            let ch = convert_from_char(ch);
            let ch = convert_to_char(ch, true);
            u.push(ch);
        }
        eprintln!("{}", u);
    }
}
