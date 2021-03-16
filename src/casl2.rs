enum Statement {
    Comment(String),
    Code {
        label: Option<String>,
        code: Code,
        comment: Option<String>,
    },
}

enum Code {
    Start,
    StartAt(String),
    End,
}
