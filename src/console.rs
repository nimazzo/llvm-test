use std::fmt::{Arguments, Display};
use std::io::Write;

#[allow(dead_code)]
#[derive(Copy, Clone)]
pub struct Console {
    pub quiet: bool,
    pub verbose: bool,
}

#[allow(dead_code)]
impl Console {
    pub fn quiet() -> Self {
        Self {
            quiet: true,
            verbose: false,
        }
    }
    pub fn normal() -> Self {
        Self {
            quiet: false,
            verbose: false,
        }
    }
    pub fn verbose() -> Self {
        Self {
            quiet: false,
            verbose: true,
        }
    }

    pub fn println(&self, s: impl Display) {
        if !self.quiet {
            println!("{}", s);
        }
    }

    pub fn force_println(&self, s: impl Display) {
        println!("{}", s);
    }

    pub fn println_verbose(&self, s: impl Display) {
        if self.verbose {
            println!("{}", s);
        }
    }
}

impl Write for Console {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if !self.quiet {
            return Ok(0);
        }
        std::io::stdout().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if !self.quiet {
            return Ok(());
        }
        std::io::stdout().flush()
    }

    fn write_fmt(&mut self, fmt: Arguments<'_>) -> std::io::Result<()> {
        if !self.quiet {
            return Ok(());
        }
        std::io::stdout().write_fmt(fmt)
    }
}
