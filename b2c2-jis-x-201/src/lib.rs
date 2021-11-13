// b2c2-jis-x-201 crate
// author: Leonardone @ NEETSDKASU

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
    for ch in s.chars() {
        if ch.is_ascii() {
            ret.push(ch);
        } else if let Some(index) = HIRAGANA.chars().position(|x| x == ch) {
            ret.push(KANA_HALF_WIDE.chars().nth(index).unwrap_or(ch));
        } else if let Some(index) = KATAKANA.chars().position(|x| x == ch) {
            ret.push(KANA_HALF_WIDE.chars().nth(index).unwrap_or(ch));
        } else if let Some(index) = HIRAGANA_DAKUON.chars().position(|x| x == ch) {
            let mut iter = KANA_DAKUON_HALF_WIDE.chars().skip(index * 2);
            let ch1 = iter.next();
            let ch2 = iter.next();
            if let (Some(ch1), Some(ch2)) = (ch1, ch2) {
                ret.push(ch1);
                ret.push(ch2);
            } else {
                ret.push(ch);
            }
        } else if let Some(index) = KATAKANA_DAKUON.chars().position(|x| x == ch) {
            let mut iter = KANA_DAKUON_HALF_WIDE.chars().skip(index * 2);
            let ch1 = iter.next();
            let ch2 = iter.next();
            if let (Some(ch1), Some(ch2)) = (ch1, ch2) {
                ret.push(ch1);
                ret.push(ch2);
            } else {
                ret.push(ch);
            }
        } else if let Some(index) = ASCII.chars().position(|x| x == ch) {
            ret.push((b' ' + index as u8) as char);
        } else if let Some(index) = KIGOU.chars().position(|x| x == ch) {
            ret.push(KIGOU_HALF_WIDE.chars().nth(index).unwrap_or(ch));
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
