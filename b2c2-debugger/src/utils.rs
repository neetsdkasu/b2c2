// b2c2-debugger crate::utils
// author: Leonardone @ NEETSDKASU

pub(super) fn is_valid_boolean(v: u16) -> bool {
    v == 0 || v == 0xFFFF
}

pub(super) fn edit_distance(str1: &str, str2: &str) -> usize {
    let str1 = str1.as_bytes();
    let str2 = str2.as_bytes();
    let mut dp = vec![vec![0; str1.len() + 1]; 2];
    for (i, v) in dp[0].iter_mut().enumerate() {
        *v = i;
    }
    for (i, ch2) in str2.iter().enumerate() {
        let prev = i & 1;
        let next = prev ^ 1;
        dp[next][0] = i + 1;
        for (k, ch1) in str1.iter().enumerate() {
            let cost = if ch1 == ch2 { 0 } else { 1 };
            dp[next][k + 1] = (dp[next][k] + 1)
                .min(dp[prev][k] + cost)
                .min(dp[prev][k + 1] + 1);
        }
    }
    dp[str2.len() & 1][str1.len()]
}
