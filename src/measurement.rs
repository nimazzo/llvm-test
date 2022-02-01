use std::fmt::{Display, Formatter};
use std::time::Instant;

pub struct Timer {
    start: Option<Instant>,
    measurements: Vec<Measurement>,
    formats: Formats,
}

struct Formats {
    max_desc_len: usize,
    max_seconds: f64,
    max_seconds_len: usize,
    seconds_prec: usize,
    max_millis: u128,
    max_millis_len: usize,
    max_nanos: u128,
    max_nanos_len: usize,
    ident: usize,
}

impl Default for Formats {
    fn default() -> Self {
        Self {
            max_desc_len: 20,
            max_seconds: 9999.0,
            max_seconds_len: 13,
            seconds_prec: 5,
            max_millis: 99999999,
            max_millis_len: 10,
            max_nanos: 9999999999999,
            max_nanos_len: 15,
            ident: 8,
        }
    }
}

struct Measurement {
    name: &'static str,
    secs: f64,
    millis: u128,
    nanos: u128,
}

impl Measurement {
    fn new(name: &'static str) -> Self {
        Self {
            name,
            secs: 0.0,
            millis: 0,
            nanos: 0,
        }
    }
}

impl Timer {
    pub fn new() -> Self {
        Self {
            start: None,
            measurements: vec![],
            formats: Default::default(),
        }
    }

    pub fn start(&mut self, description: &'static str) {
        self.start = Some(Instant::now());
        self.measurements.push(Measurement::new(description));
    }

    pub fn stop(&mut self) {
        let elapsed = self.start.unwrap().elapsed();
        let nanos = elapsed.as_nanos();
        let millis = elapsed.as_millis();
        let secs = elapsed.as_secs_f64();
        let last = self.measurements.last_mut().unwrap();
        last.secs = secs;
        last.millis = millis;
        last.nanos = nanos;

        self.start = None;
    }
}

impl Timer {
    fn format_description(&self, desc: &'static str) -> String {
        let str_len = desc.len();
        return if str_len > self.formats.max_desc_len {
            format!("{}..", &desc[..self.formats.max_desc_len - 2])
        } else {
            let buffer = self.formats.max_desc_len - str_len;
            let left = buffer / 2;
            let right = buffer - left;
            format!("{}{}{}", " ".repeat(left), desc, " ".repeat(right))
        };
    }

    fn format_secs(&self, secs: f64) -> String {
        if secs > self.formats.max_seconds {
            return format!(
                "{}.{}",
                self.formats.max_seconds as u64,
                "9".repeat(self.formats.seconds_prec)
            );
        }
        let secs_format = format!("{:.*}", self.formats.seconds_prec, secs);
        let buffer = self.formats.max_seconds_len - secs_format.len();
        let left = buffer / 2;
        let right = buffer - left;
        format!("{}{}{}", " ".repeat(left), secs_format, " ".repeat(right))
    }

    fn format_millis(&self, millis: u128) -> String {
        if millis > self.formats.max_millis {
            return "9".repeat(self.formats.max_millis_len);
        }
        let millis_format = millis.to_string();
        let buffer = self.formats.max_millis_len - millis_format.len();
        let left = buffer / 2;
        let right = buffer - left;
        format!("{}{}{}", " ".repeat(left), millis_format, " ".repeat(right))
    }

    fn format_nanos(&self, nanos: u128) -> String {
        if nanos > self.formats.max_nanos {
            return "9".repeat(self.formats.max_nanos_len);
        }
        let nanos_format = nanos.to_string();
        let buffer = self.formats.max_nanos_len - nanos_format.len();
        let left = buffer / 2;
        let right = buffer - left;
        format!("{}{}{}", " ".repeat(left), nanos_format, " ".repeat(right))
    }

    fn header(&self) -> String {
        "
                             Timing Information
        ╔════════════════════╦═════════════╦══════════╦═══════════════╗
        ║  Compilation Step  ║   Seconds   ║  Millis  ║     Nanos     ║
        ╠════════════════════╬═════════════╬══════════╬═══════════════╣"
            .to_string()
    }

    fn footer(&self) -> String {
        format!(
            "{}╚{}╩{}╩{}╩{}╝",
            " ".repeat(self.formats.ident),
            "═".repeat(self.formats.max_desc_len),
            "═".repeat(self.formats.max_seconds_len),
            "═".repeat(self.formats.max_millis_len),
            "═".repeat(self.formats.max_nanos_len),
        )
    }

    fn format_line(&self, m: &Measurement) -> String {
        format!(
            "{}║{}║{}║{}║{}║",
            " ".repeat(self.formats.ident),
            self.format_description(m.name),
            self.format_secs(m.secs),
            self.format_millis(m.millis),
            self.format_nanos(m.nanos)
        )
    }
}
/*  ╔═════╦═════╗
 *  ╠═════╬═════╣
 *  ║     ║     ║
 *  ╚═════╩═════╝ */
impl Display for Timer {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.header())?;
        for measurement in &self.measurements {
            writeln!(f, "{}", self.format_line(measurement))?;
        }
        writeln!(f, "{}", self.footer())?;
        Ok(())
    }
}
