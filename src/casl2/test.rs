// crate::casl2::test

use super::*;

const SRC: &str = "\
MUL       START
; MULTIPLY
; GR0 = GR1 * GR2
; SUPPORT (0 <= GR1 < 256) && (0<= GR2 < 256)
          PUSH      0,GR1
          PUSH      0,GR2
          XOR       GR0,GR0        ; GR0 = 0
LOOP      SRL       GR1,1          ; GR1 >>= 1
          JOV       ADD
          JZE       NEXT
          POP       GR2
          POP       GR1
          RET
ADD       ADDL      GR0,GR2        ; GR0 += GR2
NEXT      SLL       GR2,1          ; GR2 <<= 1
          JUMP      LOOP
          END
";

#[test]
fn it_works() {
    let statements = get_statements();
    let program = get_program();
    let parsed_statements = parse(SRC).unwrap();

    assert_eq!(statements, program.statements);
    assert_eq!(statements, parsed_statements);

    assert_eq!(SRC, &program.to_string());
}

fn get_statements() -> Vec<Statement> {
    use super::Command as Cmd;
    use super::IndexRegister as Idx;
    use super::Register as Reg;
    use super::Statement as Stmt;

    vec![
        // MUL       START
        Stmt::labeled("MUL", Cmd::Start { entry_point: None }),
        // ; MULTIPLY
        Stmt::comment("MULTIPLY"),
        // ; GR0 = GR1 * GR2
        Stmt::comment("GR0 = GR1 * GR2"),
        // ; SUPPORT (0 <= GR1 < 256) && (0<= GR2 < 256)
        Stmt::comment("SUPPORT (0 <= GR1 < 256) && (0<= GR2 < 256)"),
        // PUSH      0,GR1
        Stmt::code(Cmd::P {
            code: P::Push,
            adr: Adr::Dec(0),
            x: Some(Idx::Gr1),
        }),
        // PUSH      0,GR2
        Stmt::code(Cmd::P {
            code: P::Push,
            adr: Adr::Dec(0),
            x: Some(Idx::Gr2),
        }),
        // XOR       GR0,GR0        ; GR0 = 0
        Stmt::code_with_comment(
            Cmd::R {
                code: R::Xor,
                r1: Reg::Gr0,
                r2: Reg::Gr0,
            },
            "GR0 = 0",
        ),
        // LOOP      SRL       GR1,1          ; GR1 >>= 1
        Stmt::labeled_with_comment(
            "LOOP",
            Cmd::A {
                code: A::Srl,
                r: Reg::Gr1,
                adr: Adr::Dec(1),
                x: None,
            },
            "GR1 >>= 1",
        ),
        // JOV       ADD
        Stmt::code(Cmd::P {
            code: P::Jov,
            adr: Adr::label("ADD"),
            x: None,
        }),
        // JZE       NEXT
        Stmt::code(Cmd::P {
            code: P::Jze,
            adr: Adr::label("NEXT"),
            x: None,
        }),
        // POP       GR2
        Stmt::code(Cmd::Pop { r: Reg::Gr2 }),
        // POP       GR1
        Stmt::code(Cmd::Pop { r: Reg::Gr1 }),
        // RET
        Stmt::code(Cmd::Ret),
        // ADD       ADDL      GR0,GR2        ; GR0 += GR2
        Stmt::labeled_with_comment(
            "ADD",
            Cmd::R {
                code: R::Addl,
                r1: Reg::Gr0,
                r2: Reg::Gr2,
            },
            "GR0 += GR2",
        ),
        // NEXT      SLL       GR2,1          ; GR2 <<= 1
        Stmt::labeled_with_comment(
            "NEXT",
            Cmd::A {
                code: A::Sll,
                r: Reg::Gr2,
                adr: Adr::Dec(1),
                x: None,
            },
            "GR2 <<= 1",
        ),
        // JUMP      LOOP
        Stmt::code(Cmd::P {
            code: P::Jump,
            adr: Adr::label("LOOP"),
            x: None,
        }),
        // END
        Stmt::code(Cmd::End),
    ]
}

fn get_program() -> Program {
    use super::Command as Cmd;
    use super::IndexRegister as Idx;
    use super::Register as Reg;

    // MUL    START
    Builder::new("MUL")
        // ; MULTIPLY
        .comment("MULTIPLY")
        // ; GR0 = GR1 * GR2
        .comment("GR0 = GR1 * GR2")
        // ; SUPPORT (0 <= GR1 < 256) && (0<= GR2 < 256)
        .comment("SUPPORT (0 <= GR1 < 256) && (0<= GR2 < 256)")
        // PUSH 0,GR1
        .code(Cmd::P {
            code: P::Push,
            adr: Adr::Dec(0),
            x: Some(Idx::Gr1),
        })
        // PUSH 0,GR2
        .code(Cmd::P {
            code: P::Push,
            adr: Adr::Dec(0),
            x: Some(Idx::Gr2),
        })
        // XOR  GR0,GR0    ; GR0 = 0
        .code_with_comment(
            Cmd::R {
                code: R::Xor,
                r1: Reg::Gr0,
                r2: Reg::Gr0,
            },
            "GR0 = 0",
        )
        // LOOP      SRL       GR1,1          ; GR1 >>= 1
        .label("LOOP")
        .code_with_comment(
            Cmd::A {
                code: A::Srl,
                r: Reg::Gr1,
                adr: Adr::Dec(1),
                x: None,
            },
            "GR1 >>= 1",
        )
        // JOV       ADD
        .code(Cmd::P {
            code: P::Jov,
            adr: Adr::label("ADD"),
            x: None,
        })
        // JZE       NEXT
        .code(Cmd::P {
            code: P::Jze,
            adr: Adr::label("NEXT"),
            x: None,
        })
        // POP       GR2
        .code(Cmd::Pop { r: Reg::Gr2 })
        // POP       GR1
        .code(Cmd::Pop { r: Reg::Gr1 })
        // RET
        .code(Cmd::Ret)
        // ADD       ADDL      GR0,GR2        ; GR0 += GR2
        .label("ADD")
        .code_with_comment(
            Cmd::R {
                code: R::Addl,
                r1: Reg::Gr0,
                r2: Reg::Gr2,
            },
            "GR0 += GR2",
        )
        // NEXT      SLL       GR2,1          ; GR2 <<= 1
        .label("NEXT")
        .code_with_comment(
            Cmd::A {
                code: A::Sll,
                r: Reg::Gr2,
                adr: Adr::Dec(1),
                x: None,
            },
            "GR2 <<= 1",
        )
        // JUMP      LOOP
        .code(Cmd::P {
            code: P::Jump,
            adr: Adr::label("LOOP"),
            x: None,
        })
        // END
        .end()
}
