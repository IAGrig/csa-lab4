pub mod isa;

pub mod test_utils {
    use std::{fs, path::Path, process::Command};

    pub fn run_xxd(path: &Path) -> String {
        let output = Command::new("xxd")
            .arg(path)
            .output()
            .expect("Failed to run xxd");
        String::from_utf8(output.stdout).expect("xxd produced invalid UTF-8")
    }

    pub fn read_file_or_empty(path: &Path) -> String {
        fs::read_to_string(path).unwrap_or_default()
    }

    pub fn truncate_log(log: &str, max_lines: usize) -> String {
        let lines: Vec<&str> = log.lines().collect();
        if lines.len() <= max_lines {
            return log.to_string();
        }

        let half = max_lines / 2;
        let mut result = lines[..half].join("\n");
        result.push_str("\n...\n");
        result.push_str(&lines[lines.len() - half..].join("\n"));
        result
    }
}
