// b2c2-flag crate
// author: Leonardone @ NEETSDKASU

use b2c2_compiler as compiler;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[derive(Clone, Default)]
pub struct Flags {
    pub src_file: Option<String>,
    pub compiler: compiler::Flag,
    pub statistics: bool,
    pub dst_dir: Option<String>,
    pub run_debugger: Option<DebugTarget>,
}

#[derive(Clone, Copy)]
pub enum DebugTarget {
    Casl2,
    Basic,
    NonStep,
}

impl Flags {
    // 出力先ディレクトリの生成
    pub fn create_dst_dir(&self) -> io::Result<PathBuf> {
        if let Some(dir) = self.dst_dir.as_ref() {
            let path = Path::new(dir);
            if !path.exists() || !path.is_dir() {
                fs::create_dir_all(dir)?;
            }
            Ok(path.to_path_buf())
        } else {
            Ok(PathBuf::new())
        }
    }
}
