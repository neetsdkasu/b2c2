// crate::compiler::subroutine

use crate::casl2;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum Id {
    FuncAbs,
    FuncCInt,
    FuncMax,
    FuncMin,
    FuncCStrArgBool,
    FuncCStrArgInt,
    FuncSpace,
    UtilCompareInt,
    UtilCompareStr,
    UtilConcatStr,
    UtilCopyStr,
    UtilDivMod,
    UtilFill,
    UtilMul,
    UtilSafeIndex,
}

impl Id {
    pub fn label(&self) -> String {
        format!("C{}", *self as isize)
    }
}

pub struct Src {
    pub dependencies: Vec<Id>,
    pub statements: Vec<casl2::Statement>,
}

pub trait Gen {
    fn var_label(&mut self) -> String;
    fn jump_label(&mut self) -> String;
}

// サブルーチンのソースコードと依存関係を取得
pub fn get_src<T: Gen>(gen: &mut T, id: Id) -> Src {
    match id {
        Id::FuncAbs => get_func_abs(gen, id),
        Id::FuncCInt => get_func_cint(gen, id),
        Id::FuncMax => get_func_max(gen, id),
        Id::FuncMin => get_func_min(gen, id),
        Id::FuncCStrArgBool => get_func_cstr_arg_bool(gen, id),
        Id::FuncCStrArgInt => get_func_cstr_arg_int(gen, id),
        Id::FuncSpace => get_func_space(gen, id),
        Id::UtilCompareInt => get_util_compare_int(gen, id),
        Id::UtilCompareStr => get_util_compare_str(gen, id),
        Id::UtilConcatStr => get_util_concat_str(gen, id),
        Id::UtilCopyStr => get_util_copy_str(gen, id),
        Id::UtilDivMod => get_util_div_mod(gen, id),
        Id::UtilFill => get_util_fill(gen, id),
        Id::UtilMul => get_util_mul(gen, id),
        Id::UtilSafeIndex => get_util_safe_index(gen, id),
    }
}

// Func: Abs
fn get_func_abs<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 .. value1
    // GR0 .. ret = abs(value1)
    Src {
        dependencies: Vec::new(),
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog} LD    GR0,GR1
       JMI   {mi}
       RET
{mi}   XOR   GR0,GR0
       SUBA  GR0,GR1
       RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            mi = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Func: Max
fn get_func_max<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 .. value1
    // GR2 .. value2
    // GR0 .. ret = max(value1, value2)
    Src {
        dependencies: Vec::new(),
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog} CPA   GR1,GR2
       JMI   {mi}
       LD    GR0,GR1
       RET
{mi}   LD    GR0,GR2
       RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            mi = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Func: Min
fn get_func_min<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 .. value1
    // GR2 .. value2
    // GR0 .. ret = min(value1, value2)
    Src {
        dependencies: Vec::new(),
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog} CPA   GR1,GR2
       JMI   {mi}
       LD    GR0,GR2
       RET
{mi}   LD    GR0,GR1
       RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            mi = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Func: CInt
fn get_func_cint<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 .. adr of s_buf
    // GR2 .. s_len
    // GR0 .. ret
    Src {
        dependencies: Vec::new(),
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog} PUSH  0,GR1
       PUSH  0,GR2
       PUSH  0,GR3
       PUSH  0,GR4
       PUSH  0,GR5
       ADDL  GR2,GR1
       XOR   GR0,GR0
       XOR   GR4,GR4
       CPL   GR1,GR2
       JZE   {ret}
       LD    GR3,0,GR1
       CPL   GR3,='+'
       JNZ   {mi}
       LAD   GR1,1,GR1
       JUMP  {read}
{mi}   CPL   GR3,='-'
       JNZ   {read}
       LAD   GR4,-1
       LAD   GR1,1,GR1
{read} CPL   GR1,GR2
       JZE   {ret}
       LD    GR3,0,GR1
       SUBL  GR3,='0'
       JMI   {ret}
       CPL   GR3,=9
       JPL   {ret}
       LD    GR5,GR0
       SLL   GR0,3
       ADDL  GR0,GR5
       ADDL  GR0,GR5
       ADDL  GR0,GR3
       LAD   GR1,1,GR1
       JUMP  {read}
{ret}  XOR   GR0,GR4
       SUBL  GR0,GR4
       POP   GR5
       POP   GR4
       POP   GR3
       POP   GR2
       POP   GR1
       RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            ret = gen.jump_label(),
            read = gen.jump_label(),
            mi = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Util: Compare Int
fn get_util_compare_int<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 lhs
    // GR2 rhs
    // GR0 ... -1 if lhs < rhs , 0 if lhs = rhs, 1 if lhs > rhs
    Src {
        dependencies: Vec::new(),
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog}  XOR    GR0,GR0
        CPA    GR1,GR2
        JMI    {minus}
        JPL    {plus}
        RET
{minus} LAD    GR0,#FFFF
{plus}  OR     GR0,=1
        RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            minus = gen.jump_label(),
            plus = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Util: Safe Index
fn get_util_safe_index<T: Gen>(_gen: &mut T, id: Id) -> Src {
    // GR1 index of array
    // GR2 size of array
    // GR0 = Max(0, Min(GR1, GR2 - 1))
    Src {
        dependencies: vec![Id::FuncMax, Id::FuncMin],
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog}  PUSH   0,GR1
        PUSH   0,GR2
        LAD    GR2,-1,GR2
        CALL   {min}
        XOR    GR1,GR1
        LD     GR2,GR0
        CALL   {max}
        POP    GR2
        POP    GR1
        RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            min = Id::FuncMin.label(),
            max = Id::FuncMax.label()
        ))
        .unwrap(),
    }
}

// Util: Div Mod
fn get_util_div_mod<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR2 割られる数 (分子)
    // GR3 割る数 (分母)
    // GR0 商    = GR2 \ GR3
    // GR1 余り   = GR2 Mod GR3
    Src {
        dependencies: vec![Id::FuncAbs, Id::UtilMul],
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog}  AND   GR3,GR3
        JNZ   {ok}
        XOR   GR0,GR0
        LAD   GR1,-1
        RET
{ok}    PUSH  0,GR2
        PUSH  0,GR3
        PUSH  0,GR4
        PUSH  0,GR5
        LD    GR4,GR2
        LD    GR1,GR2
        CALL  {abs}
        LD    GR5,GR0
        LD    GR1,GR3
        CALL  {abs}
        LD    GR1,GR0
        LAD   GR0,1
{shift} ADDL  GR1,GR1
        JOV   {pre}
        ADDL  GR0,GR0
        JUMP  {shift}
{pre}   SRL   GR1,1
        LAD   GR1,#8000,GR1
        XOR   GR2,GR2
{cycle} CPL   GR5,GR1
        JMI   {next}
        SUBL  GR5,GR1
        ADDL  GR2,GR0
{next}  SRL   GR0,1
        JZE   {ret}
        SRL   GR1,1
        JUMP  {cycle}
{ret}   LD    GR5,GR4
        XOR   GR5,GR3
        SRA   GR5,15
        XOR   GR2,GR5
        SUBA  GR2,GR5
        CALL  {mul}
        LD    GR1,GR4
        SUBA  GR1,GR0
        LD    GR0,GR2
        POP   GR5
        POP   GR4
        POP   GR3
        POP   GR2
        RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            abs = Id::FuncAbs.label(),
            mul = Id::UtilMul.label(),
            ok = gen.jump_label(),
            shift = gen.jump_label(),
            pre = gen.jump_label(),
            cycle = gen.jump_label(),
            next = gen.jump_label(),
            ret = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Util: Mul
fn get_util_mul<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR2 * GR3
    // GR0 積の下位16ビット  (GR2 * GR3) & 0x0000FFFF
    // GR1 積の上位16ビット ((GR2 * GR3) & 0xFFFF0000) >> 16
    Src {
        dependencies: vec![Id::FuncAbs, Id::UtilMul],
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog}   PUSH  0,GR2
         PUSH  0,GR3
         PUSH  0,GR4
         PUSH  0,GR5
         XOR   GR0,GR0
         XOR   GR1,GR1
         LD    GR4,GR2
         LD    GR5,GR3
{cycle1} SRL   GR2,1
         JOV   {add1}
         JNZ   {next1}
         JUMP  {cycle2}
{add1}   ADDL  GR0,GR3
         JOV   {raise1}
         JUMP  {next1}
{raise1} LAD   GR1,1,GR1
{next1}  SLL   GR3,1
         JUMP  {cycle1}
{cycle2} SRL   GR5,1
         SLL   GR4,1
         JOV   {add2}
         JNZ   {cycle2}
         JUMP  {ret}
{add2}   ADDL  GR1,GR5
         JUMP  {cycle2}
{ret}    POP   GR5
         POP   GR4
         POP   GR3
         POP   GR2
         RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            cycle1 = gen.jump_label(),
            add1 = gen.jump_label(),
            raise1 = gen.jump_label(),
            next1 = gen.jump_label(),
            cycle2 = gen.jump_label(),
            add2 = gen.jump_label(),
            ret = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Func: CStr (bool)
fn get_func_cstr_arg_bool<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 .. adr of s_buf
    // GR2 .. adr of s_len
    // GR3 .. value (boolean)
    Src {
        dependencies: vec![Id::UtilCopyStr],
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog}   PUSH  0,GR3
         PUSH  0,GR4
         AND   GR3,GR3
         LAD   GR3,='FalseTrue'
         LAD   GR4,5
         JZE   {ret}
         ADDL  GR3,GR4
         LAD   GR4,4
{ret}    CALL  {copy}
         POP   GR4
         POP   GR3
         RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            copy = Id::UtilCopyStr.label(),
            ret = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Func: CStr (int)
fn get_func_cstr_arg_int<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 .. adr of s_buf
    // GR2 .. adr of s_len
    // GR3 .. value (integer)
    Src {
        dependencies: vec![Id::UtilDivMod, Id::UtilCopyStr],
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog}   CPL   GR3,=#8000
         JNZ   {zero}
         PUSH  0,GR3
         PUSH  0,GR4
         LAD   GR3,='-32768'
         LAD   GR4,6
         CALL  {copystr}
         POP   GR4
         POP   GR3
         RET
{zero}   AND   GR3,GR3
         JNZ   {init}
         LAD   GR3,1
         ST    GR3,0,GR2
         LD    GR3,='0'
         ST    GR3,0,GR1
         XOR   GR3,GR3
         RET
{init}   PUSH  0,GR1
         PUSH  0,GR2
         PUSH  0,GR3
         PUSH  0,GR4
         PUSH  0,GR5
         JPL   {start}
         LD    GR4,='-'
         ST    GR4,0,GR1
         LAD   GR1,1,GR1
         XOR   GR3,=#FFFF
         LAD   GR3,1,GR3
{start}  LAD   GR4,{temp}
         LD    GR5,GR1
         LD    GR2,GR3
         LAD   GR3,10
{cycle}  CALL  {rem}
         ADDL  GR1,='0'
         ST    GR1,0,GR4
         LAD   GR4,1,GR4
         LD    GR2,GR0
         JPL   {cycle}
         LAD   GR2,{temp}
         LAD   GR4,-1,GR4
{copy}   LD    GR1,0,GR4
         ST    GR1,0,GR5
         LAD   GR5,1,GR5
         LAD   GR4,-1,GR4
         CPL   GR4,GR2
         JPL   {copy}
         JZE   {copy}
         LD    GR0,GR5
         POP   GR5
         POP   GR4
         POP   GR3
         POP   GR2
         POP   GR1
         SUBL  GR0,GR1
         ST    GR0,0,GR2
         RET
{temp}   DS    6
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            rem = Id::UtilDivMod.label(),
            copystr = Id::UtilCopyStr.label(),
            zero = gen.jump_label(),
            init = gen.jump_label(),
            start = gen.jump_label(),
            cycle = gen.jump_label(),
            copy = gen.jump_label(),
            temp = gen.var_label()
        ))
        .unwrap(),
    }
}

// Util: Compare Str
fn get_util_compare_str<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 .. adr of s_buf (lhs)
    // GR2 .. s_len (lhs)
    // GR3 .. adr of s_buf (rhs)
    // GR4 .. s_len (rhs)
    // GR0 .. -1 if lhs < rhs, 0 if lhs == rhs, 1 if lhs > rhs
    Src {
        dependencies: Vec::new(),
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog}   PUSH  0,GR1
         PUSH  0,GR2
         PUSH  0,GR3
         PUSH  0,GR4
         PUSH  0,GR5
         XOR   GR0,GR0
{cycle}  AND   GR2,GR2
         JPL   {next}
         CPL   GR2,GR4
         JNZ   {less}
         JUMP  {ret}
{next}   AND   GR4,GR4
         JZE   {great}
         LD    GR5,0,GR1
         CPL   GR5,0,GR3
         JMI   {less}
         JPL   {great}
         LAD   GR1,1,GR1
         LAD   GR2,-1,GR2
         LAD   GR3,1,GR3
         LAD   GR4,-1,GR4
         JUMP  {cycle}
{less}   LAD   GR0,-1
{great}  OR    GR0,=1
{ret}    POP   GR5
         POP   GR4
         POP   GR3
         POP   GR2
         POP   GR1
         RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            cycle = gen.jump_label(),
            next = gen.jump_label(),
            less = gen.jump_label(),
            great = gen.jump_label(),
            ret = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Util: Copy Str
fn get_util_copy_str<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 .. adr of s_buf (dst)
    // GR2 .. adr of s_len (dst)
    // GR3 .. adr of s_buf (src)
    // GR4 .. s_len (src)
    //  copy from (GR3,GR4) to (GR1,GR2)
    Src {
        dependencies: Vec::new(),
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog}   PUSH  0,GR1
         PUSH  0,GR2
         PUSH  0,GR3
         PUSH  0,GR4
         ST    GR4,0,GR2
         AND   GR4,GR4
         JZE   {ret}
{cycle}  LD    GR2,0,GR3
         ST    GR2,0,GR1
         LAD   GR3,1,GR3
         LAD   GR1,1,GR1
         SUBA  GR4,=1
         JPL   {cycle}
{ret}    POP   GR4
         POP   GR3
         POP   GR2
         POP   GR1
         RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            cycle = gen.jump_label(),
            ret = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Util: Concat Str
fn get_util_concat_str<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 .. adr of s_buf (dst,left)
    // GR2 .. adr of s_len (dst)
    // GR3 .. adr of s_buf (src,right)
    // GR4 .. s_len (src)
    //   GR1 = GR1 & GR3
    //   GR2 = Min(GR2 + GR4, 256)
    Src {
        dependencies: Vec::new(),
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog}   PUSH  0,GR1
         PUSH  0,GR2
         PUSH  0,GR3
         PUSH  0,GR4
         LD    GR0,0,GR2
         LD    GR2,GR1
         ADDL  GR1,GR0
         LAD   GR2,256,GR2
         ADDL  GR4,GR3
{cycle}  CPL   GR1,GR2
         JZE   {ret}
         CPL   GR3,GR4
         JZE   {ret}
         LD    GR0,0,GR3
         ST    GR0,0,GR1
         LAD   GR1,1,GR1
         LAD   GR3,1,GR3
         JUMP  {cycle}
{ret}    LD    GR0,GR1
         POP   GR4
         POP   GR3
         POP   GR2
         POP   GR1
         SUBL  GR0,GR1
         ST    GR0,0,GR2
         RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            cycle = gen.jump_label(),
            ret = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Util: Fill
fn get_util_fill<T: Gen>(gen: &mut T, id: Id) -> Src {
    // GR1 .. adr of start point of s_buf or array
    // GR2 .. fill value
    // GR3 .. fill len
    Src {
        dependencies: Vec::new(),
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog} PUSH  0,GR1
       PUSH  0,GR2
       PUSH  0,GR3
       ADDL  GR3,GR1
{next} CPL   GR1,GR3
       JZE   {ret}
       ST    GR2,0,GR1
       LAD   GR1,1,GR1
       JUMP  {next}
{ret}  POP   GR3
       POP   GR2
       POP   GR1
       RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            next = gen.jump_label(),
            ret = gen.jump_label()
        ))
        .unwrap(),
    }
}

// Func Space
fn get_func_space<T: Gen>(_gen: &mut T, id: Id) -> Src {
    // GR1 .. adr of s_buf
    // GR2 .. adr of s_len
    // GR3 .. space len
    Src {
        dependencies: vec![Id::UtilFill, Id::UtilSafeIndex],
        statements: casl2::parse(&format!(
            r#"
                                   ; {comment}
{prog} PUSH  0,GR1
       PUSH  0,GR2
       PUSH  0,GR3
       XOR   GR1,GR3
       XOR   GR3,GR1
       XOR   GR1,GR3
       LAD   GR2,256
       CALL  {fit}
       LD    GR1,GR3
       LD    GR3,GR0
       LD    GR2,=' '
       CALL  {fill}
       LD    GR0,GR3
       POP   GR3
       POP   GR2
       POP   GR1
       ST    GR0,0,GR2
       RET
"#,
            comment = format!("{:?}", id),
            prog = id.label(),
            fit = Id::UtilSafeIndex.label(),
            fill = Id::UtilFill.label()
        ))
        .unwrap(),
    }
}