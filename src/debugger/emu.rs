use super::*;

// プログラムリスト (ファイル名, ラベル, ソースコード(コードメモリ位置, コード))
type Program = (String, String, Vec<(usize, casl2::Statement)>);

// コールスタック用に確保する最大サイズ　(1呼び出しにつき、RPUSHで7語、MEMで1語、合計8語として、再帰呼び出しの深さ200とすると2000語分程度あればよいか？)
pub(super) const CALLSTACK_MAX_SIZE: usize = 0x2000; /* 8192語 */

// プログラムコードを埋め込みを開始する位置 (0はダメ)
pub(super) const BEGIN_OF_PROGRAM: usize = 0x0100; /* 256 */

// COMET2の容量(語数)
pub(super) const MEMORY_SIZE: usize = 0x10000;

// スタックポインタの始点
// 仕様どおりにするなら最初のプログラム起動時はOSからCALLが呼ばれるはずで
// SP   <- (SP) - 1  (SP = 0xFFFD)
// (SP) <- (PR)      (0xFFFDの位置に初期PRの値が放り込まれる)
// PR   <- address
// RET命令で取り出した値が初期PRの値と同じならプログラム終了と同等になるが
// RET命令でSPが0xFFFDでは無い時に初期PRの値と同じならスタックが壊れてる(笑)
// またSPが0xFFFDのRET命令で初期PRと異なる値を得たならプログラムが暴走する(笑)
pub(super) const CALLSTACK_START_POSITION: usize = 0xFFFF;

// プログラムレジスタ(PR)の初期値
// OSがCALLでプログラムを呼び出した位置に相当する
// RET命令でこの値が取り出されたら、プログラム終了だがSP値が適切ではない場合はOSが暴走する(笑)
// 今回OSは作らないため、OSのコード実体はないため、RETでこの値が来たらエミュレータ終了である
// BEGIN_OF_PROGRAMより手前のメモリ帯にOSのコードがあると想定してる
pub(super) const INITIAL_PROGRAM_REGISTER: usize = 0x0040; // 64

#[derive(Default)]
pub(super) struct CodeInfo {
    pub(super) pos: usize,
    pub(super) mem_code: String,
    pub(super) src_code: Option<(usize, String)>,
    pub(super) src_file: Option<String>,
    pub(super) src_entry_label: Option<String>,
    pub(super) alias_labels: Vec<String>,
}

#[derive(Debug)]
pub(super) enum RuntimeError {
    NormalTermination {
        position: usize,
    },
    AbnormalTermination {
        position: usize,
        op_code: String,
    },
    ExecutePermissionDenied {
        position: usize,
    },
    AccessPermissionDenied {
        position: usize,
        op_code: String,
        address: usize,
    },
    MemoryAccessOutOfBounds {
        position: usize,
        op_code: String,
        address: usize,
    },
    InvalidOpCode {
        position: usize,
        op_code: String,
    },
    StackOverflow {
        position: usize,
        op_code: String,
        stack_pointer: usize,
    },
    StackEmpty {
        position: usize,
        op_code: String,
    },
    IoError(io::Error),
}

pub(super) struct Emulator {
    pub(super) debug_mode: bool,

    // COMET2のメモリ
    pub(super) mem: Vec<u16>,

    // レジスタ GR0～GR7
    pub(super) general_registers: Vec<u16>,

    // スタックポインタ SP
    // (CASL2からのアクセス手段はないため利便性からu16ではなくusizeで管理)
    // (OSはSPを直接操作できるような事が仕様に書かれてるが具体的手段自体は実装依存ぽい)
    // CALLやPUSHでは先にSPが更新される、つまりSPはスタックの値の存在する先頭アドレス、か
    pub(super) stack_pointer: usize,

    // プログラムレジスタ PR
    // (CALL命令でスタックに積む以外でu16である必要性はないため利便性からusizeで管理)
    // "次に"実行すべき命令のアドレスらしい
    // つまり命令をCPUに伝える前にPRは更新する必要があるぽい？(CALL/RETの仕様を考えると)
    //  CALLが  (SP) <- (PR), PR <- address
    //  RETが   PR <- ( (SP) )
    pub(super) program_register: usize,

    // フラグレジスタ FR (CASL2からの直接的なアクセス手段はないため、boolで管理)
    //  OF overflow_flag 加減算ではオーバーフロー、シフト演算では溢れビット
    //  SF sign_flag 算術か論理に関わらず符号ビットが立ってるか否からしい？
    //  ZF zero_flag 値が0ならフラグ立つ
    pub(super) overflow_flag: bool,
    pub(super) sign_flag: bool,
    pub(super) zero_flag: bool,

    // 最後に実行したコードの位置(PRの値)
    pub(super) last_run_position: usize,

    // CASL2ステップ数(step_through_code呼び出し回数)
    pub(super) step_count: u64,

    // ブレークポイント
    pub(super) break_points: Vec<bool>,

    // 各メモリ位置のコードとしての実行回数
    pub(super) execute_count: Vec<u64>,
    // 各メモリ位置の参照回数 (LDやADDLやCPAやOUTなどの命令による)
    pub(super) access_count: Vec<u64>,
    // 各メモリ位置の更新回数 (STやINの命令による)
    pub(super) update_count: Vec<u64>,

    // サブルーチンのネストの追跡用スタック(呼び出し先アドレス、戻りアドレス)
    // RETでCOMET2のスタックメモリから取り出す値がこのスタックの値と同値だったら正常とみなす(?)
    pub(super) program_stack: Vec<(usize, usize)>,
    // 異常RETの記録  (RET命令自体のメモリ位置、RET呼び出し時点のstep_count、スタックから取り出した戻りアドレス)
    pub(super) wrong_ret: Vec<(usize, u64, usize)>,

    // BASICステップ (DebugBasicStepのid)
    pub(super) basic_step: Option<u16>,
    // BASICステップ数 (DebugBasicStepの呼び出し回数)
    pub(super) basic_step_count: u32,

    // プログラムの開始点のエントリラベル
    // (START命令に引数があるとBEGIN_OF_PROGRAMにはならないこともあるため)
    pub(super) start_point: Option<String>,

    // デバッガコマンドから設定したラベル (ラベル、(メモリ位置、ラベル生成コード))
    pub(super) labels_for_debug: HashMap<String, (usize, casl2::Statement)>,

    // デバッガコマンドで生成したリテラル (ラベル、(メモリ位置、値))
    pub(super) literals_for_debug: HashMap<String, (usize, Value)>,

    // デバッガコマンドで生成したエイリアスのラベル (ラベル、(メモリ位置、値))
    pub(super) alias_labels: HashMap<String, usize>,

    // 全ラベルのリスト(ソースコード由来のみ)
    pub(super) all_label_list: Vec<(String, usize)>,

    // 挿入するプログラムの埋め込み開始位置
    pub(super) compile_pos: usize,

    // BASICコードの情報 (ソースファイル名, (プログラムエントリラベル, デバッグ用情報))
    pub(super) basic_info: HashMap<String, (String, compiler::DebugInfo)>,

    // プログラムごとのラベルとメモリ位置の対応 (プログラムエントリラベル, (ラベル, メモリ位置))
    pub(super) local_labels: HashMap<String, HashMap<String, usize>>,

    // プログラムのエントリラベルとメモリ位置の対応 (プログラムエントリラベル, メモリ位置)
    pub(super) program_labels: HashMap<String, usize>,

    // プログラムリスト (ファイル名, ラベル, ソースコード(コードメモリ位置, コード))
    pub(super) program_list: Vec<Program>,

    // 未解決ラベル(おそらく外部定義)
    pub(super) unknown_labels: HashMap<String, Vec<usize>>,

    // 読み込み済みファイル一覧
    pub(super) loaded_files: HashSet<String>,
}

impl Emulator {
    pub(super) fn new() -> Self {
        Self {
            debug_mode: true,
            mem: vec![0; MEMORY_SIZE],
            general_registers: vec![0; 8],
            stack_pointer: CALLSTACK_START_POSITION,
            program_register: INITIAL_PROGRAM_REGISTER,
            overflow_flag: false,
            sign_flag: false,
            zero_flag: false,

            break_points: vec![false; MEMORY_SIZE],

            last_run_position: INITIAL_PROGRAM_REGISTER - 2,
            step_count: 0,
            basic_step: None,
            basic_step_count: 0,
            labels_for_debug: HashMap::new(),
            literals_for_debug: HashMap::new(),
            alias_labels: HashMap::new(),
            program_stack: Vec::new(),
            wrong_ret: Vec::new(),
            execute_count: vec![0; MEMORY_SIZE],
            access_count: vec![0; MEMORY_SIZE],
            update_count: vec![0; MEMORY_SIZE],

            all_label_list: Vec::new(),
            start_point: None,
            basic_info: HashMap::new(),
            compile_pos: BEGIN_OF_PROGRAM,
            local_labels: HashMap::new(),
            program_labels: HashMap::new(),
            program_list: Vec::new(),
            unknown_labels: HashMap::new(),
            loaded_files: HashSet::new(),
        }
    }

    pub(super) fn init_to_start(&mut self, start_point: Option<usize>) {
        let position = start_point.unwrap_or_else(|| {
            self.start_point
                .as_ref()
                .and_then(|label| self.program_labels.get(label))
                .copied()
                .expect("BUG")
        });

        let call = casl2::Command::P {
            code: casl2::P::Call,
            adr: casl2::Adr::Hex(position as u16),
            x: None,
        };

        let stack_pointer = CALLSTACK_START_POSITION - 1;

        self.mem[INITIAL_PROGRAM_REGISTER - 2] = call.first_word();
        self.mem[INITIAL_PROGRAM_REGISTER - 1] = position as u16;
        self.last_run_position = INITIAL_PROGRAM_REGISTER - 2;
        self.mem[stack_pointer] = INITIAL_PROGRAM_REGISTER as u16;
        self.stack_pointer = stack_pointer;
        self.program_register = position;
        self.step_count = 0;
        self.basic_step = None;
        self.basic_step_count = 0;

        self.program_stack.clear();
        self.program_stack
            .push((position, INITIAL_PROGRAM_REGISTER));
        self.wrong_ret.clear();

        self.execute_count.fill(0);
        self.access_count.fill(0);
        self.update_count.fill(0);

        // GRとFRは不定
    }

    pub(super) fn enough_remain(&self, len: usize) -> bool {
        len < self.mem.len() - CALLSTACK_MAX_SIZE - self.compile_pos
    }
}

impl Emulator {
    pub(super) fn get_current_program(&self) -> Option<&Program> {
        self.program_list.iter().find(|(_, _, stmt)| {
            stmt.first()
                .zip(stmt.last())
                .filter(|(fp, lp)| (fp.0..=lp.0).contains(&self.program_register))
                .is_some()
        })
    }

    pub(super) fn get_code_info(&self, pos: u16) -> CodeInfo {
        use std::fmt::Write;
        let pos = pos as usize;
        let mut info = CodeInfo {
            pos,
            alias_labels: self
                .alias_labels
                .iter()
                .filter_map(|(k, p)| if *p == pos { Some(k.clone()) } else { None })
                .collect(),
            ..Default::default()
        };
        for (file, label, stmt) in self.program_list.iter() {
            if stmt.is_empty() {
                continue;
            }
            let (pos1, _) = stmt.first().unwrap();
            let (pos2, _) = stmt.last().unwrap();
            if pos < *pos1 || *pos2 < pos {
                continue;
            }
            let mut tmp = "";
            let mut index = match stmt.binary_search_by_key(&pos, |(p, _)| *p) {
                Ok(index) => index,
                Err(0) => {
                    tmp = "+ ";
                    0
                }
                Err(index) => {
                    tmp = "+ ";
                    index - 1
                }
            };
            let (pos, _) = &stmt[index];
            while index > 0 {
                let (p, _) = &stmt[index - 1];
                if p == pos {
                    index -= 1;
                } else {
                    break;
                }
            }
            for (_, code) in stmt.iter().skip(index).take_while(|(p, _)| p == pos) {
                match code {
                    casl2::Statement::Comment { .. } => {}
                    casl2::Statement::Code { command, .. } => match command {
                        casl2::Command::Start { .. } | casl2::Command::End => {}
                        _ => {
                            info.src_file = Some(file.clone());
                            info.src_entry_label = Some(label.clone());
                            info.src_code = Some((*pos, format!("{}{}", tmp, code)));
                            break;
                        }
                    },
                }
            }
            break;
        }
        if info.src_code.is_none() {
            for (key, (p, stmt)) in self.labels_for_debug.iter() {
                if *p == pos {
                    info.src_entry_label = Some(key.clone());
                    info.src_code = Some((*p, stmt.to_string()));
                    break;
                }
            }
        }
        let op_code = self.mem[pos];
        match get_op_code_size(op_code) {
            2 => {
                if let Some(adr) = self.mem.get(pos + 1).cloned() {
                    let bp = if self.break_points[pos] || self.break_points[pos + 1] {
                        "*"
                    } else {
                        " "
                    };
                    let s = get_op_code_form(op_code, Some(adr));
                    write!(&mut info.mem_code, " #{:04X}: {} {}", pos, bp, s).unwrap();
                    info
                } else {
                    let bp = if self.break_points[pos] { "*" } else { " " };
                    let s = get_op_code_form(op_code, None);
                    write!(&mut info.mem_code, " #{:04X}: {} {}", pos, bp, s).unwrap();
                    info
                }
            }
            _ => {
                let bp = if self.break_points[pos] { "*" } else { " " };
                let s = get_op_code_form(op_code, None);
                write!(&mut info.mem_code, " #{:04X}: {} {}", pos, bp, s).unwrap();
                info
            }
        }
    }
}

impl Emulator {
    pub(super) fn get_address_by_label_str(&self, label: &str) -> Result<usize, String> {
        let mut tokenizer = casl2::Tokenizer::new(label);
        match tokenizer.extended_label() {
            Err(msg) => Err(msg.to_string()),
            Ok(None) => Err("引数が不正です".to_string()),
            Ok(Some(lb)) => {
                if tokenizer.rest().is_empty() {
                    lb.get_address(self)
                } else {
                    Err("引数が不正です".to_string())
                }
            }
        }
    }
}

impl Emulator {
    pub(super) fn get_label(&self, pos: u16) -> (Vec<String>, Vec<String>) {
        let pos = pos as usize;
        let mut plabel = Vec::new();
        for (lb, p) in self.all_label_list.iter() {
            if *p == pos {
                plabel.push(lb.clone());
            }
        }
        let mut dlabel = Vec::new();
        for (lb, (p, _)) in self.labels_for_debug.iter() {
            if *p == pos {
                dlabel.push(lb.clone());
            }
        }
        for (lb, p) in self.alias_labels.iter() {
            if *p == pos {
                dlabel.push(lb.clone());
            }
        }
        (plabel, dlabel)
    }
}

impl Emulator {
    pub(super) fn make_literal_int(&mut self, v: u16) -> Result<usize, &str> {
        let key = v.to_string();
        if let Some((pos, _)) = self.literals_for_debug.get(&key) {
            return Ok(*pos);
        }
        if !self.enough_remain(1) {
            return Err("メモリ不足でリテラルを生成できません");
        }
        let pos = self.compile_pos;
        self.mem[pos] = v;
        self.compile_pos += 1;
        self.literals_for_debug.insert(key, (pos, Value::Int(v)));
        Ok(pos)
    }

    pub(super) fn make_literal_str(&mut self, s: &str) -> Result<usize, &str> {
        if s.is_empty() {
            return Err("空の文字定数のリテラルは生成できません");
        }
        let bs = jis_x_201::convert_kana_wide_full_to_half(s)
            .chars()
            .map(jis_x_201::convert_from_char)
            .collect::<Vec<_>>();
        let s = bs
            .iter()
            .map(|ch| jis_x_201::convert_to_char(*ch, true))
            .collect::<String>();
        let key = format!("'{}", s);
        if let Some((pos, _)) = self.literals_for_debug.get(&key) {
            return Ok(*pos);
        }
        if !self.enough_remain(bs.len()) {
            return Err("メモリ不足でリテラルを生成できません");
        }
        let pos = self.compile_pos;
        for (i, ch) in bs.iter().enumerate() {
            self.mem[pos + i] = *ch as u16;
        }
        self.compile_pos += bs.len();
        self.literals_for_debug.insert(key, (pos, Value::Str(s)));
        Ok(pos)
    }

    pub(super) fn resolve_dummy_code(&mut self, label: String) -> Result<(), String> {
        if !self.enough_remain(1) {
            return Err("メモリ不足でダミープログラムをロードできませんでした".into());
        }
        let label_pos = self.compile_pos;
        self.compile_pos += 1;
        self.mem[label_pos] = casl2::Command::Ret.first_word();
        if let Some(pos_list) = self.unknown_labels.remove(&label) {
            for pos in pos_list {
                self.mem[pos] = label_pos as u16;
            }
        }
        self.program_labels.insert(label, label_pos);
        Ok(())
    }

    pub(super) fn suggest_load_file(
        &self,
        dst_dir: &Path,
        src_dir: &Path,
    ) -> io::Result<Option<(String, Vec<String>)>> {
        let target_label = match self.unknown_labels.keys().next() {
            Some(label) => label.clone(),
            None => return Ok(None),
        };
        let lc_target_label = target_label.to_ascii_lowercase();
        let target_name = format!("{}.cas", target_label);
        let lc_target_name = target_name.to_ascii_lowercase();
        let bas_target_name = format!("{}.bas", target_label);
        let lc_bas_target_name = bas_target_name.to_ascii_lowercase();
        let targets = [&target_label, &target_name, &bas_target_name];
        let lc_targets = [&lc_target_label, &lc_target_name, &lc_bas_target_name];
        let mut list = Vec::new();
        for (i, dir) in [dst_dir, src_dir].iter().enumerate() {
            if i == 0 && dst_dir == src_dir {
                continue;
            }
            for entry in dir.read_dir()? {
                let entry = entry?;
                if !entry.path().is_file() {
                    continue;
                }
                let path = entry.path().to_string_lossy().into_owned();
                if self.loaded_files.contains(&path) {
                    continue;
                }
                let file_name = entry.file_name().to_string_lossy().into_owned();
                let lc_file_name = file_name.to_ascii_lowercase();
                let dist1 = targets
                    .iter()
                    .map(|target| edit_distance(&target, &file_name))
                    .min()
                    .unwrap();
                let dist2 = lc_targets
                    .iter()
                    .map(|target| edit_distance(&target, &lc_file_name))
                    .min()
                    .unwrap();
                let dist = dist1.min(dist2);
                list.push((dist, i, path));
            }
        }
        list.sort();
        let list: Vec<_> = list.into_iter().map(|(.., p)| p).collect();
        Ok(Some((target_label, list)))
    }
}

impl Emulator {
    pub(super) fn step_through_code<R, W>(
        &mut self,
        stdin: &mut R,
        stdout: &mut W,
    ) -> Result<(), RuntimeError>
    where
        R: io::BufRead,
        W: io::Write,
    {
        let pr = self.program_register;
        if pr >= 0xFFFF {
            return Err(RuntimeError::ExecutePermissionDenied { position: pr });
        }
        self.execute_count[pr] += 1;
        self.step_count += 1;
        self.basic_step = None;
        self.last_run_position = pr;
        self.program_register += 1;
        let op_code = self.mem[pr];
        let r1 = ((op_code >> 4) & 0xF) as usize;
        let r2 = (op_code & 0xF) as usize;
        let op_code_size = get_op_code_size(op_code);

        if op_code_size == 0 {
            return Err(RuntimeError::InvalidOpCode {
                position: pr,
                op_code: get_op_code_form(op_code, None),
            });
        }

        if op_code_size == 1 {
            if !(0..8).contains(&r1) || !(0..8).contains(&r2) {
                return Err(RuntimeError::InvalidOpCode {
                    position: pr,
                    op_code: get_op_code_form(op_code, None),
                });
            }
            match ((op_code >> 8) & 0xFF) as u8 {
                0x14 => {
                    // LD r1,r2  (OF=0,SF,ZF)
                    let r2_value = self.general_registers[r2];
                    self.general_registers[r1] = r2_value;
                    self.overflow_flag = false;
                    self.sign_flag = (r2_value as i16) < 0;
                    self.zero_flag = r2_value == 0;
                }
                0x24 => {
                    // ADDA r1,r2 (OF,SF,ZF)
                    let r1_value = self.general_registers[r1] as i16;
                    let r2_value = self.general_registers[r2] as i16;
                    let (value, overflow) = r1_value.overflowing_add(r2_value);
                    self.general_registers[r1] = value as u16;
                    self.overflow_flag = overflow;
                    self.sign_flag = value < 0;
                    self.zero_flag = value == 0;
                }
                0x26 => {
                    // ADDL r1,r2 (OF,SF,ZF)
                    let r1_value = self.general_registers[r1];
                    let r2_value = self.general_registers[r2];
                    let (value, overflow) = r1_value.overflowing_add(r2_value);
                    self.general_registers[r1] = value;
                    self.overflow_flag = overflow;
                    self.sign_flag = (value as i16) < 0;
                    self.zero_flag = value == 0;
                }
                0x25 => {
                    // SUBA r1,r2 (OF,SF,ZF)
                    let r1_value = self.general_registers[r1] as i16;
                    let r2_value = self.general_registers[r2] as i16;
                    let (value, overflow) = r1_value.overflowing_sub(r2_value);
                    self.general_registers[r1] = value as u16;
                    self.overflow_flag = overflow;
                    self.sign_flag = value < 0;
                    self.zero_flag = value == 0;
                }
                0x27 => {
                    // SUBL r1,r2 (OF,SF,ZF)
                    let r1_value = self.general_registers[r1];
                    let r2_value = self.general_registers[r2];
                    let (value, overflow) = r1_value.overflowing_sub(r2_value);
                    self.general_registers[r1] = value;
                    self.overflow_flag = overflow;
                    self.sign_flag = (value as i16) < 0;
                    self.zero_flag = value == 0;
                }
                0x34 => {
                    // AND r1,r2 (OF=0,SF,ZF)
                    let r2_value = self.general_registers[r2];
                    let value = self.general_registers[r1] & r2_value;
                    self.general_registers[r1] = value;
                    self.overflow_flag = false;
                    self.sign_flag = (value as i16) < 0;
                    self.zero_flag = value == 0;
                }
                0x35 => {
                    // OR r1,r2 (OF=0,SF,ZF)
                    let r2_value = self.general_registers[r2];
                    let value = self.general_registers[r1] | r2_value;
                    self.general_registers[r1] = value;
                    self.overflow_flag = false;
                    self.sign_flag = (value as i16) < 0;
                    self.zero_flag = value == 0;
                }
                0x36 => {
                    // XOR r1,r2 (OF=0,SF,ZF)
                    let r2_value = self.general_registers[r2];
                    let value = self.general_registers[r1] ^ r2_value;
                    self.general_registers[r1] = value;
                    self.overflow_flag = false;
                    self.sign_flag = (value as i16) < 0;
                    self.zero_flag = value == 0;
                }
                0x44 => {
                    // CPA r1,r2 (OF=0,SF,ZF)
                    let r1_value = self.general_registers[r1] as i16;
                    let r2_value = self.general_registers[r2] as i16;
                    self.overflow_flag = false;
                    self.sign_flag = r1_value < r2_value;
                    self.zero_flag = r1_value == r2_value;
                }
                0x45 => {
                    // CPL r1,r2 (OF=0,SF,ZF)
                    let r1_value = self.general_registers[r1];
                    let r2_value = self.general_registers[r2];
                    self.overflow_flag = false;
                    self.sign_flag = r1_value < r2_value;
                    self.zero_flag = r1_value == r2_value;
                }
                0x71 if r2 == 0 => {
                    // POP r
                    if self.stack_pointer >= CALLSTACK_START_POSITION {
                        return Err(RuntimeError::StackEmpty {
                            position: pr,
                            op_code: get_op_code_form(op_code, None),
                        });
                    }
                    let value = self.mem[self.stack_pointer];
                    self.stack_pointer += 1;
                    self.general_registers[r1] = value;
                }
                0x81 if r1 == 0 && r2 == 0 => {
                    // RET
                    if self.stack_pointer >= CALLSTACK_START_POSITION {
                        return Err(RuntimeError::StackEmpty {
                            position: pr,
                            op_code: get_op_code_form(op_code, None),
                        });
                    }
                    let adr = self.mem[self.stack_pointer] as usize;
                    if self
                        .program_stack
                        .last()
                        .filter(|(_, r)| *r == adr)
                        .is_some()
                    {
                        self.program_stack.pop();
                    } else {
                        self.wrong_ret.push((pr, self.step_count, adr));
                    }
                    self.stack_pointer += 1;
                    self.program_register = adr;
                    if adr == INITIAL_PROGRAM_REGISTER {
                        if self.stack_pointer == CALLSTACK_START_POSITION {
                            return Err(RuntimeError::NormalTermination { position: pr });
                        } else {
                            return Err(RuntimeError::AbnormalTermination {
                                position: pr,
                                op_code: get_op_code_form(op_code, None),
                            });
                        }
                    }
                }
                0x00 if r1 == 0 && r2 == 0 => {
                    // NOP
                }
                _ => {
                    return Err(RuntimeError::InvalidOpCode {
                        position: pr,
                        op_code: get_op_code_form(op_code, None),
                    })
                }
            }
            return Ok(());
        }

        assert_eq!(op_code_size, 2);

        let adr = self.mem[pr + 1];
        self.program_register += 1;

        if !(0..8).contains(&r1) || !(0..8).contains(&r2) {
            return Err(RuntimeError::InvalidOpCode {
                position: pr,
                op_code: get_op_code_form(op_code, Some(adr)),
            });
        }

        match ((op_code >> 8) & 0xFF) as u8 {
            0x12 => {
                // LAD r,adr,x
                let value = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                self.general_registers[r1] = value;
                return Ok(());
            }
            0x50 => {
                // SLA r,adr,x
                let shift = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                let r1_value = self.general_registers[r1];
                let r1_value = if (0..16).contains(&shift) {
                    self.overflow_flag = (((r1_value as u32) << shift) & 0x10000) != 0;
                    (r1_value & 0x8000) | ((r1_value << shift) & 0x7FFF)
                } else {
                    self.overflow_flag = false;
                    0
                };
                self.general_registers[r1] = r1_value;
                self.sign_flag = (r1_value as i16) < 0;
                self.zero_flag = r1_value == 0;
                return Ok(());
            }
            0x51 => {
                // SRA r,adr,x
                let shift = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                let r1_value = self.general_registers[r1] as i16;
                let r1_value = if (0..16).contains(&shift) {
                    self.overflow_flag = ((((r1_value as i32) << 1) >> shift) & 1) != 0;
                    r1_value >> shift
                } else {
                    self.overflow_flag = r1_value < 0;
                    (r1_value >> 15) >> 1
                };
                self.general_registers[r1] = r1_value as u16;
                self.sign_flag = r1_value < 0;
                self.zero_flag = r1_value == 0;
                return Ok(());
            }
            0x52 => {
                // SLL r,adr,x
                let shift = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                let r1_value = self.general_registers[r1] as u32;
                let r1_value = if (0..=16).contains(&shift) {
                    self.overflow_flag = ((r1_value << shift) & 0x10000) != 0;
                    (r1_value << shift) as u16
                } else {
                    self.overflow_flag = false;
                    0
                };
                self.general_registers[r1] = r1_value;
                self.sign_flag = (r1_value as i16) < 0;
                self.zero_flag = r1_value == 0;
                return Ok(());
            }
            0x53 => {
                // SRL r,adr,x
                let shift = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                let r1_value = self.general_registers[r1] as u32;
                let r1_value = if (0..=16).contains(&shift) {
                    self.overflow_flag = (((r1_value << 1) >> shift) & 1) != 0;
                    (r1_value >> shift) as u16
                } else {
                    self.overflow_flag = false;
                    0
                };
                self.general_registers[r1] = r1_value;
                self.sign_flag = (r1_value as i16) < 0;
                self.zero_flag = r1_value == 0;
                return Ok(());
            }
            0x70 => {
                // PUSH adr,x
                if r1 != 0 {
                    return Err(RuntimeError::InvalidOpCode {
                        position: pr,
                        op_code: get_op_code_form(op_code, Some(adr)),
                    });
                }
                let value = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                let sp = self.stack_pointer - 1;
                if sp < self.mem.len() - CALLSTACK_MAX_SIZE {
                    return Err(RuntimeError::StackOverflow {
                        position: pr,
                        op_code: get_op_code_form(op_code, Some(adr)),
                        stack_pointer: self.stack_pointer,
                    });
                }
                self.stack_pointer = sp;
                self.mem[sp] = value;
                return Ok(());
            }
            0xF0 => {
                // SVC adr,x
                if r1 != 0 {
                    return Err(RuntimeError::InvalidOpCode {
                        position: pr,
                        op_code: get_op_code_form(op_code, Some(adr)),
                    });
                }
                let value = adr.wrapping_add(if r2 == 0 {
                    0
                } else {
                    self.general_registers[r2]
                });
                if value == 1 {
                    // IN
                    let pos = self.general_registers[1] as usize;
                    let len = self.general_registers[2] as usize;
                    if self.debug_mode {
                        write!(stdout, "[IN] ? ")?;
                    } else {
                        write!(stdout, "? ")?;
                    }
                    stdout.flush()?;
                    self.update_count[len] += 1;
                    let mut line = String::new();
                    match stdin.read_line(&mut line)? {
                        0 => self.mem[len] = (-1_i16) as u16,
                        _ => {
                            let line = line.lines().next().unwrap();
                            let line = jis_x_201::convert_kana_wide_full_to_half(&line);
                            self.mem[len] = line.chars().count().min(256) as u16;
                            for (i, ch) in line.chars().enumerate().take(256) {
                                if pos + i > 0xFFFF {
                                    return Err(RuntimeError::MemoryAccessOutOfBounds {
                                        position: pr,
                                        op_code: get_op_code_form(op_code, Some(adr)),
                                        address: pos + i,
                                    });
                                }
                                let ch = jis_x_201::convert_from_char(ch) as u16;
                                self.mem[pos + i] = ch;
                                self.update_count[pos + i] += 1;
                            }
                        }
                    }
                } else if value == 2 {
                    // OUT
                    let pos = self.general_registers[1] as usize;
                    let len = self.general_registers[2] as usize;
                    let len = self.mem[len] as usize;
                    self.access_count[len] += 1;
                    let mut line = String::new();
                    for i in 0..len {
                        if pos + i > 0xFFFF {
                            return Err(RuntimeError::MemoryAccessOutOfBounds {
                                position: pr,
                                op_code: get_op_code_form(op_code, Some(adr)),
                                address: len,
                            });
                        }
                        let ch = (self.mem[pos + i] & 0xFF) as u8;
                        line.push(jis_x_201::convert_to_char(ch, true));
                        self.access_count[pos + i] += 1;
                    }
                    if self.debug_mode {
                        writeln!(stdout, "[OUT] {}", line)?;
                    } else {
                        writeln!(stdout, "{}", line)?;
                    }
                    stdout.flush()?;
                }
                // SVC後のGR,FRは不定なので、値を保持しないという仕様にした(雑)
                self.general_registers[0] ^=
                    (self.general_registers[1] << 1) ^ (self.general_registers[2] >> 1);
                self.general_registers[1] ^=
                    (self.general_registers[2] << 1) ^ (self.general_registers[3] >> 1);
                self.general_registers[2] ^=
                    (self.general_registers[3] << 1) ^ (self.general_registers[4] >> 1);
                self.general_registers[3] ^=
                    (self.general_registers[4] << 1) ^ (self.general_registers[5] >> 1);
                self.general_registers[4] ^=
                    (self.general_registers[5] << 1) ^ (self.general_registers[6] >> 1);
                self.general_registers[5] ^=
                    (self.general_registers[6] << 1) ^ (self.general_registers[7] >> 1);
                self.general_registers[6] ^=
                    (self.general_registers[7] << 1) ^ (self.general_registers[0] >> 1);
                self.general_registers[7] ^=
                    (self.general_registers[0] << 1) ^ (self.general_registers[3] >> 1);
                self.overflow_flag = (self.general_registers[3] & 4) == 0;
                self.sign_flag = (self.general_registers[5] & 4) == 0;
                self.zero_flag = (self.general_registers[7] & 4) == 0;
                return Ok(());
            }
            0xE0 => {
                // DebugBasicStep id
                if r1 != 0 || r2 != 0 {
                    return Err(RuntimeError::InvalidOpCode {
                        position: pr,
                        op_code: get_op_code_form(op_code, Some(adr)),
                    });
                }
                self.basic_step = Some(adr);
                self.basic_step_count += 1;
                return Ok(());
            }
            _ => {}
        }

        let access = adr as usize
            + if r2 == 0 {
                0
            } else {
                self.general_registers[r2] as usize
            };

        if access > 0xFFFF {
            return Err(RuntimeError::MemoryAccessOutOfBounds {
                position: pr,
                op_code: get_op_code_form(op_code, Some(adr)),
                address: access,
            });
        }

        match ((op_code >> 8) & 0xFF) as u8 {
            0x10 => {
                // LD r,adr,x (OF=0,SF,ZF)
                let value = self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x11 => {
                // ST r,adr,x
                let r1_value = self.general_registers[r1];
                self.mem[access] = r1_value;
                self.update_count[access] += 1;
                return Ok(());
            }
            0x20 => {
                // ADDA r,adr,x (OF,SF,ZF)
                let (value, overflow) =
                    (self.general_registers[r1] as i16).overflowing_add(self.mem[access] as i16);
                self.general_registers[r1] = value as u16;
                self.overflow_flag = overflow;
                self.sign_flag = value < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x22 => {
                // ADDL r,adr,x
                let (value, overflow) =
                    self.general_registers[r1].overflowing_add(self.mem[access]);
                self.general_registers[r1] = value;
                self.overflow_flag = overflow;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x21 => {
                // SUBA r,adr,x
                let (value, overflow) =
                    (self.general_registers[r1] as i16).overflowing_sub(self.mem[access] as i16);
                self.general_registers[r1] = value as u16;
                self.overflow_flag = overflow;
                self.sign_flag = value < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x23 => {
                // SUBL r,adr,x
                let (value, overflow) =
                    self.general_registers[r1].overflowing_sub(self.mem[access]);
                self.general_registers[r1] = value;
                self.overflow_flag = overflow;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x30 => {
                // AND r,adr,x (OF=0,SF,ZF)
                let value = self.general_registers[r1] & self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x31 => {
                // OR r,adr,x (OF=0,SF,ZF)
                let value = self.general_registers[r1] | self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x32 => {
                // XOR r,adr,x (OF=0,SF,ZF)
                let value = self.general_registers[r1] ^ self.mem[access];
                self.general_registers[r1] = value;
                self.overflow_flag = false;
                self.sign_flag = (value as i16) < 0;
                self.zero_flag = value == 0;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x40 => {
                // CPA r,adr,x (OF=0,SF,ZF)
                let r1_value = self.general_registers[r1] as i16;
                let value = self.mem[access] as i16;
                self.overflow_flag = false;
                self.sign_flag = r1_value < value;
                self.zero_flag = r1_value == value;
                self.access_count[access] += 1;
                return Ok(());
            }
            0x41 => {
                // CPL r,adr,x (OF=0,SF,ZF)
                let r1_value = self.general_registers[r1];
                let value = self.mem[access];
                self.overflow_flag = false;
                self.sign_flag = r1_value < value;
                self.zero_flag = r1_value == value;
                self.access_count[access] += 1;
                return Ok(());
            }
            _ => {}
        }

        if r1 != 0 {
            return Err(RuntimeError::InvalidOpCode {
                position: pr,
                op_code: get_op_code_form(op_code, Some(adr)),
            });
        }

        match ((op_code >> 8) & 0xFF) as u8 {
            0x65 => {
                // JPL adr,x
                if !self.sign_flag && !self.zero_flag {
                    self.program_register = access;
                }
            }
            0x61 => {
                // JMI adr,x
                if self.sign_flag {
                    self.program_register = access;
                }
            }
            0x62 => {
                // JNZ adr,x
                if !self.zero_flag {
                    self.program_register = access;
                }
            }
            0x63 => {
                // JZE adr,x
                if self.zero_flag {
                    self.program_register = access;
                }
            }
            0x66 => {
                // JOV adr,x
                if self.overflow_flag {
                    self.program_register = access;
                }
            }
            0x64 => {
                // JUMP adr,x
                self.program_register = access;
            }
            0x80 => {
                // CALL adr,x
                let sp = self.stack_pointer - 1;
                if sp < self.mem.len() - CALLSTACK_MAX_SIZE {
                    return Err(RuntimeError::StackOverflow {
                        position: pr,
                        op_code: get_op_code_form(op_code, Some(adr)),
                        stack_pointer: self.stack_pointer,
                    });
                }
                self.stack_pointer = sp;
                let pr = self.program_register;
                self.mem[sp] = pr as u16;
                self.program_register = access;
                self.program_stack.push((access, pr));
            }
            _ => {
                // UNKNOWN CODE
                return Err(RuntimeError::InvalidOpCode {
                    position: pr,
                    op_code: get_op_code_form(op_code, Some(adr)),
                });
            }
        }
        Ok(())
    }
}

impl Emulator {
    pub(super) fn compile_basic(
        &mut self,
        src_file: &str,
        flags: &Flags,
        save: bool,
    ) -> io::Result<i32> {
        let path = Path::new(src_file);
        if !path.exists() || !path.is_file() {
            eprintln!("ファイルが見つかりません ({})", src_file);
            return Ok(200);
        }
        if path.metadata()?.len() > 1_000_000 {
            eprintln!("ファイルサイズが大きすぎます ({})", src_file);
            return Ok(200);
        }

        let text = fs::read_to_string(src_file)?;

        let basic_src = match parser::parse(text.as_bytes())? {
            Ok(src) => src,
            Err(error) => {
                eprintln!("{:?}", error);
                let line_number = if error.line_number == 0 {
                    0
                } else {
                    error.line_number - 1
                };
                if let Some(line) = text.lines().nth(line_number) {
                    eprintln!("{}", line);
                    eprintln!("{ch:>pos$}", ch = "^", pos = error.position);
                }
                return Ok(4);
            }
        };

        let casl2_src_list = match compiler::compile_for_debugger(&basic_src, &flags.compiler) {
            Err(error) => {
                eprintln!("CompileError {{ {} }}", error);
                return Ok(5);
            }
            Ok((debug_info, list)) => {
                if let Some((name, _)) = list.last() {
                    if self.start_point.is_none() {
                        self.start_point = Some(name.clone());
                    }
                    self.basic_info
                        .insert(src_file.into(), (name.clone(), debug_info));
                } else {
                    unreachable!("BUG");
                };
                list
            }
        };

        let mut path = flags.create_dst_dir()?;

        // EOF,ALLOC
        for option in basic_src.iter().filter_map(|stmt| {
            if let parser::Statement::CompileOption { option } = stmt {
                Some(option)
            } else {
                None
            }
        }) {
            match option {
                parser::CompileOption::Eof { common: true } => {
                    let src = compiler::subroutine::get_util_eof_store_code();
                    if save {
                        self.save_casl2_src(&mut path, "EOF", &src, flags.statistics, false)?;
                    }
                    if !self.program_labels.contains_key(&"EOF".to_string()) {
                        if let Err(msg) = self.compile("EOF.cas", "EOF".into(), src) {
                            eprintln!("CompileError{{ {} }}", msg);
                            return Ok(100);
                        }
                    }
                }
                parser::CompileOption::Allocator {
                    enabled: true,
                    common: true,
                    size,
                } => {
                    let src = compiler::subroutine::get_util_allocator_code_for_common(*size);
                    if save {
                        self.save_casl2_src(&mut path, "ALLOC", &src, flags.statistics, false)?;
                    }
                    if !self.program_labels.contains_key(&"ALLOC".to_string()) {
                        if let Err(msg) = self.compile("ALLOC.cas", "ALLOC".into(), src) {
                            eprintln!("CompileError{{ {} }}", msg);
                            return Ok(100);
                        }
                    }
                }
                _ => {}
            }
        }

        for (name, casl2_src) in casl2_src_list {
            if save {
                let overwrite = compiler::utils::is_valid_program_name(&name);
                self.save_casl2_src(&mut path, &name, &casl2_src, flags.statistics, overwrite)?;
            }

            if self.program_labels.contains_key(&name) {
                continue;
            }

            if let Err(msg) = self.compile(src_file, name, casl2_src) {
                eprintln!("CompileError{{ {} }}", msg);
                return Ok(100);
            }
        }

        self.loaded_files
            .insert(Path::new(src_file).to_string_lossy().into_owned());

        Ok(0)
    }
}

impl Emulator {
    pub(super) fn save_casl2_src(
        &mut self,
        path: &mut PathBuf,
        name: &str,
        casl2_src: &[casl2::Statement],
        statistics: bool,
        overwrite: bool,
    ) -> io::Result<()> {
        let file_name = format!("{}.cas", name);
        path.push(file_name);
        {
            self.loaded_files
                .insert(path.to_string_lossy().into_owned());

            if overwrite || !path.exists() || !path.is_file() {
                let mut dst_file = fs::File::create(&path)?;

                for stmt in casl2_src.iter() {
                    writeln!(&mut dst_file, "{}", stmt)?;
                }

                dst_file.flush()?;
                if path.exists() && path.is_file() {
                    eprintln!("生成されたファイル(上書): {}", path.display());
                } else {
                    eprintln!("生成されたファイル: {}", path.display());
                }
            } else {
                eprintln!(
                    "既存するためファイル生成をスキップしました: {}",
                    path.display()
                );
            }
        }
        path.pop();

        if statistics {
            let file_name = format!("{}.stat.txt", name);
            path.push(file_name);
            self.loaded_files
                .insert(path.to_string_lossy().into_owned());
            if overwrite || !path.exists() {
                let statistics = stat::analyze(casl2_src);
                fs::write(&path, statistics)?;
                if path.exists() && path.is_file() {
                    eprintln!("生成されたファイル(上書): {}", path.display());
                } else {
                    eprintln!("生成されたファイル: {}", path.display());
                }
            }
            path.pop();
        }

        Ok(())
    }
}

impl Emulator {
    pub(super) fn compile_casl2(
        &mut self,
        src_file: &str,
        flags: &Flags,
        save: bool,
    ) -> io::Result<i32> {
        let path = Path::new(src_file);
        if !path.exists() || !path.is_file() {
            eprintln!("ファイルが見つかりません ({})", src_file);
            return Ok(200);
        }
        if path.metadata()?.len() > 1_000_000 {
            eprintln!("ファイルサイズが大きすぎます ({})", src_file);
            return Ok(200);
        }
        let text = fs::read_to_string(src_file)?;

        let casl2_src = match casl2::parse(text.as_str()) {
            Ok(src) => src,
            Err(error) => {
                eprintln!("{:?}", error);
                return Ok(6);
            }
        };

        if let Some(msg) = casl2::utils::find_syntax_error(&casl2_src) {
            eprintln!("SyntaxError{{ {} }}", msg);
            return Ok(7);
        }

        let casl2_src = if let Some(new_name) = flags.compiler.program_name.as_ref() {
            if !casl2::Label::from(new_name).is_valid() {
                eprintln!("ERROR: 禁止されている名前です  ( {} )", new_name);
                return Ok(8);
            }
            let old_name = casl2::utils::get_program_name(&casl2_src).expect("BUG");
            match casl2::utils::change_label(&casl2_src, old_name, new_name.as_str()) {
                Some(src) => src,
                None => {
                    eprintln!(
                        "ERROR: ソースコード内で既に使用されている名前です ( {} )",
                        new_name
                    );
                    return Ok(9);
                }
            }
        } else {
            casl2_src
        };

        let casl2_src_list = flags.compiler.apply(casl2_src);

        if let Some((name, _)) = casl2_src_list.last() {
            if self.start_point.is_none() {
                self.start_point = Some(name.clone());
            }
        }

        let mut path = flags.create_dst_dir()?;

        if save {
            let backup = src_file.to_string() + ".backup";
            let backup = Path::new(&backup);
            if backup.parent() == Some(&path) {
                if let Some(file) = backup.file_name() {
                    path.push(file);
                    let _copy_size = fs::copy(src_file, &path)?;
                    eprintln!("生成されたファイル: {}", path.display());
                    path.pop();
                }
            }
        }

        for (name, casl2_src) in casl2_src_list {
            if save {
                let overwrite = compiler::utils::is_valid_program_name(&name);
                self.save_casl2_src(&mut path, &name, &casl2_src, flags.statistics, overwrite)?;
            }

            if self.program_labels.contains_key(&name) {
                continue;
            }

            if let Err(msg) = self.compile(src_file, name, casl2_src) {
                eprintln!("CompileError{{ {} }}", msg);
                return Ok(100);
            }
        }

        self.loaded_files
            .insert(Path::new(src_file).to_string_lossy().into_owned());

        Ok(0)
    }
}

impl Emulator {
    pub(super) fn compile(
        &mut self,
        src_file: &str,
        name: String,
        src: Vec<casl2::Statement>,
    ) -> Result<(), String> {
        let mut code = Vec::new();
        let mut labels = HashMap::new();
        let mut literals = HashMap::new();
        let mut label_list = Vec::new();
        let mut end_label: Option<String> = None;

        for stmt in src {
            let pos = self.compile_pos;
            if let casl2::Statement::Code { label, command, .. } = &stmt {
                let len = command.len();
                if !self.enough_remain(len) {
                    return Err("メモリ不足でプログラムをロードできませんでした".into());
                }
                self.compile_pos += len;
                if let Some(label) = label {
                    match command {
                        casl2::Command::Start { .. } => {}
                        casl2::Command::End => {
                            end_label = Some(label.as_string().clone());
                        }
                        _ => {
                            labels.insert(label.as_string().clone(), pos);
                            label_list.push((format!("{}:{}", name, label.as_str()), pos));
                        }
                    }
                }
                match command {
                    casl2::Command::A { adr, .. } | casl2::Command::P { adr, .. } => {
                        literals.insert(adr.clone(), 0_usize);
                    }
                    _ => {}
                }
                self.compile_command(pos, command);
            }
            code.push((pos, stmt));
        }

        let mut lit_stmt = Vec::new();

        for (adr, pos) in literals.iter_mut() {
            match adr {
                casl2::Adr::LiteralDec(d) => {
                    if !self.enough_remain(1) {
                        return Err("メモリ不足でプログラムをロードできませんでした".into());
                    }
                    *pos = self.compile_pos;
                    self.mem[*pos] = *d as u16;
                    self.compile_pos += 1;
                    let stmt = format!(" DC {0} ; ={0}", d);
                    lit_stmt.push((*pos, casl2::parse(&stmt).unwrap().pop().unwrap()));
                }
                casl2::Adr::LiteralHex(h) => {
                    if !self.enough_remain(1) {
                        return Err("メモリ不足でプログラムをロードできませんでした".into());
                    }
                    *pos = self.compile_pos;
                    self.mem[*pos] = *h;
                    self.compile_pos += 1;
                    let stmt = format!(" DC #{0:04X} ; =#{0:04X}", h);
                    lit_stmt.push((*pos, casl2::parse(&stmt).unwrap().pop().unwrap()));
                }
                casl2::Adr::LiteralStr(ls) => {
                    let s = jis_x_201::convert_kana_wide_full_to_half(&ls);
                    let len = s.chars().count();
                    if !self.enough_remain(len) {
                        return Err("メモリ不足でプログラムをロードできませんでした".into());
                    }
                    *pos = self.compile_pos;
                    for (i, ch) in s.chars().enumerate() {
                        let ch = jis_x_201::convert_from_char(ch);
                        self.mem[*pos + i] = ch as u16;
                    }
                    self.compile_pos += len;
                    let stmt = format!(" DC '{0}' ; ='{0}'", ls.replace('\'', "''"));
                    lit_stmt.push((*pos, casl2::parse(&stmt).unwrap().pop().unwrap()));
                }
                _ => {}
            }
        }

        if let Some(end_index) = code.iter().position(|(_, stmt)| {
            matches!(
                stmt,
                casl2::Statement::Code {
                    command: casl2::Command::End,
                    ..
                }
            )
        }) {
            let mut rest = code.split_off(end_index);
            code.extend(lit_stmt);
            let pos = self.compile_pos - 1;
            if let Some(label) = end_label {
                labels.insert(label.clone(), pos);
                label_list.push((format!("{}:{}", name, label.as_str()), pos));
            }
            rest.iter_mut().for_each(|(p, _)| *p = pos);
            code.extend(rest);
        }

        for (mempos, stmt) in code.iter() {
            let command = if let casl2::Statement::Code { command, .. } = stmt {
                command
            } else {
                continue;
            };
            match command {
                casl2::Command::Start { entry_point } => {
                    if let Some(label) = entry_point {
                        let label = label.as_str().to_string();
                        if let Some(pos) = labels.get(&label) {
                            self.program_labels.insert(name.clone(), *pos);
                        } else {
                            return Err(format!("指定されたラベルが見つかりません ( {} )", label));
                        }
                    } else {
                        self.program_labels.insert(name.clone(), *mempos);
                    }
                }
                casl2::Command::Dc { constants } => {
                    let mut pos = *mempos;
                    for cnst in constants.iter() {
                        match cnst {
                            casl2::Constant::Dec(d) => {
                                self.mem[pos] = *d as u16;
                                pos += 1;
                            }
                            casl2::Constant::Hex(h) => {
                                self.mem[pos] = *h;
                                pos += 1;
                            }
                            casl2::Constant::Str(s) => {
                                let s = jis_x_201::convert_kana_wide_full_to_half(&s);
                                for ch in s.chars() {
                                    self.mem[pos] = jis_x_201::convert_from_char(ch) as u16;
                                    pos += 1;
                                }
                            }
                            casl2::Constant::Label(label) => {
                                let label = label.as_string();
                                if let Some(label_pos) = labels.get(label) {
                                    self.mem[pos] = *label_pos as u16;
                                } else if let Some(label_pos) =
                                    self.program_labels.get(label).copied()
                                {
                                    self.mem[pos] = label_pos as u16;
                                } else {
                                    self.unknown_labels
                                        .entry(label.clone())
                                        .or_insert_with(Vec::new)
                                        .push(pos);
                                }
                                pos += 1;
                            }
                        }
                    }
                }
                casl2::Command::In { pos, len } | casl2::Command::Out { pos, len } => {
                    let pos = pos.as_string();
                    let len = len.as_string();
                    if let Some(label_pos) = labels.get(pos) {
                        self.mem[*mempos + 18] = *label_pos as u16;
                    } else if let Some(label_pos) = self.program_labels.get(pos).copied() {
                        self.mem[*mempos + 18] = label_pos as u16;
                    } else {
                        self.unknown_labels
                            .entry(pos.clone())
                            .or_insert_with(Vec::new)
                            .push(*mempos + 18);
                    }
                    if let Some(label_pos) = labels.get(len) {
                        self.mem[*mempos + 20] = *label_pos as u16;
                    } else if let Some(label_pos) = self.program_labels.get(len).copied() {
                        self.mem[*mempos + 20] = label_pos as u16;
                    } else {
                        self.unknown_labels
                            .entry(len.clone())
                            .or_insert_with(Vec::new)
                            .push(*mempos + 20);
                    }
                }
                casl2::Command::A { adr, .. } | casl2::Command::P { adr, .. } => match adr {
                    casl2::Adr::Dec(d) => self.mem[*mempos + 1] = *d as u16,
                    casl2::Adr::Hex(h) => self.mem[*mempos + 1] = *h,
                    casl2::Adr::Label(label) => {
                        let label = label.as_string();
                        if let Some(label_pos) = labels.get(label) {
                            self.mem[*mempos + 1] = *label_pos as u16;
                        } else if let Some(label_pos) = self.program_labels.get(label).copied() {
                            self.mem[*mempos + 1] = label_pos as u16;
                        } else {
                            self.unknown_labels
                                .entry(label.clone())
                                .or_insert_with(Vec::new)
                                .push(*mempos + 1);
                        }
                    }
                    casl2::Adr::LiteralDec(_)
                    | casl2::Adr::LiteralHex(_)
                    | casl2::Adr::LiteralStr(_) => {
                        let label_pos = literals.get(adr).expect("BUG");
                        self.mem[*mempos + 1] = *label_pos as u16;
                    }
                },
                _ => {}
            }
        }

        let program_pos = *self.program_labels.get(&name).expect("BUG");

        if let Some(pos_list) = self.unknown_labels.remove(&name) {
            for pos in pos_list {
                self.mem[pos] = program_pos as u16;
            }
        }

        // 分かりにくい条件式…
        if label_list
            .first()
            .filter(|(_, pos)| *pos < program_pos)
            .is_none()
        {
            self.all_label_list.push((name.clone(), program_pos));
            self.all_label_list.extend(label_list);
        } else {
            for (label, pos) in label_list {
                if program_pos == pos {
                    self.all_label_list.push((name.clone(), program_pos));
                }
                self.all_label_list.push((label, pos));
            }
        }

        self.local_labels.insert(name.clone(), labels);
        self.program_list.push((src_file.into(), name, code));

        Ok(())
    }

    pub(super) fn compile_command(&mut self, pos: usize, command: &casl2::Command) {
        match command {
            casl2::Command::Start { .. }
            | casl2::Command::End
            | casl2::Command::Ds { .. }
            | casl2::Command::Dc { .. } => {}
            casl2::Command::In { .. } => {
                let src = casl2::parse(
                    r#" PUSH 0,GR1
                        PUSH 0,GR2
                        PUSH 0,GR3
                        PUSH 0,GR4
                        PUSH 0,GR5
                        PUSH 0,GR6
                        PUSH 0,GR7
                        LD  GR1,GR0
                        PUSH 0,GR1
                        LAD GR1,0
                        LAD GR2,0
                        SVC 1
                        POP GR1
                        LD  GR0,GR1
                        POP GR7
                        POP GR6
                        POP GR5
                        POP GR4
                        POP GR3
                        POP GR2
                        POP GR1
                    "#,
                )
                .unwrap();
                let mut pos = pos;
                for stmt in src.iter() {
                    if let casl2::Statement::Code { command, .. } = stmt {
                        let len = command.len();
                        self.mem[pos] = command.first_word();
                        match command {
                            casl2::Command::P { adr, .. } | casl2::Command::A { adr, .. } => {
                                match adr {
                                    casl2::Adr::Dec(d) => self.mem[pos + 1] = *d as u16,
                                    casl2::Adr::Hex(h) => self.mem[pos + 1] = *h,
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                        pos += len;
                    }
                }
            }
            casl2::Command::Out { .. } => {
                let src = casl2::parse(
                    r#" PUSH 0,GR1
                        PUSH 0,GR2
                        PUSH 0,GR3
                        PUSH 0,GR4
                        PUSH 0,GR5
                        PUSH 0,GR6
                        PUSH 0,GR7
                        LD  GR1,GR0
                        PUSH 0,GR1
                        LAD GR1,0
                        LAD GR2,0
                        SVC 2
                        POP GR1
                        LD  GR0,GR1
                        POP GR7
                        POP GR6
                        POP GR5
                        POP GR4
                        POP GR3
                        POP GR2
                        POP GR1
                    "#,
                )
                .unwrap();
                let mut pos = pos;
                for stmt in src.iter() {
                    if let casl2::Statement::Code { command, .. } = stmt {
                        let len = command.len();
                        self.mem[pos] = command.first_word();
                        match command {
                            casl2::Command::P { adr, .. } | casl2::Command::A { adr, .. } => {
                                match adr {
                                    casl2::Adr::Dec(d) => self.mem[pos + 1] = *d as u16,
                                    casl2::Adr::Hex(h) => self.mem[pos + 1] = *h,
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                        pos += len;
                    }
                }
            }
            casl2::Command::Rpush => {
                use casl2::IndexRegister::*;
                let mut pos = pos;
                for &x in &[Gr1, Gr2, Gr3, Gr4, Gr5, Gr6, Gr7] {
                    let cmd = casl2::Command::P {
                        code: casl2::P::Push,
                        adr: casl2::Adr::Dec(0),
                        x: Some(x),
                    };
                    self.mem[pos] = cmd.first_word();
                    self.mem[pos + 1] = 0;
                    pos += 2;
                }
            }
            casl2::Command::Rpop => {
                use casl2::Register::*;
                let mut pos = pos;
                for &r in &[Gr7, Gr6, Gr5, Gr4, Gr3, Gr2, Gr1] {
                    let cmd = casl2::Command::Pop { r };
                    self.mem[pos] = cmd.first_word();
                    pos += 1;
                }
            }
            casl2::Command::R { .. }
            | casl2::Command::A { .. }
            | casl2::Command::P { .. }
            | casl2::Command::Pop { .. }
            | casl2::Command::Ret
            | casl2::Command::Nop => {
                self.mem[pos] = command.first_word();
            }
            casl2::Command::DebugBasicStep { id } => {
                self.mem[pos] = command.first_word();
                self.mem[pos + 1] = *id as u16;
            }
        }
    }
}
