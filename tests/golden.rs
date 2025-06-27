use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use csa_lab4::test_utils;

use pretty_assertions::assert_eq;
use tempfile::tempdir;

#[test]
fn run_all_tests() {
    fs::read_dir("tests/golden")
        .expect("Failed to read golden directory")
        .for_each(|entry| {
            let path = entry.ok().unwrap().path();
            if path.extension().unwrap() == "yml" {
                run_golden_test(path);
            }
        });
}

fn run_golden_test(test_file: PathBuf) {
    let yaml_content = fs::read_to_string(test_file).expect("Failed to read test file");
    let spec: serde_yaml::Value = serde_yaml::from_str(&yaml_content).expect("Invalid YAML");

    let temp_dir = tempdir().expect("Failed to create temp directory");
    let temp_path = temp_dir.path();

    let source_path = temp_path.join("source.lsp");
    let out_path = temp_path.join("out");
    let input_path = temp_path.join("input");
    let out_code_bin_path = temp_path.join("out.bin");
    let out_code_hex_path = temp_path.join("out.bin.hex");
    let out_data_bin_path = temp_path.join("out.data.bin");
    let result_data_path = Path::new("result-data.bin");

    fs::write(&source_path, spec["in_source"].as_str().unwrap()).expect("Failed to write source");
    fs::write(&input_path, spec["in_stdin"].as_str().unwrap()).expect("Failed to write input");

    let translator_output = Command::new("cargo")
        .arg("run")
        .arg("--bin")
        .arg("translator")
        .arg(&source_path)
        .arg(&out_path)
        .output()
        .expect("Failed to run translator");

    assert!(
        translator_output.status.success(),
        "Translator failed: {}",
        String::from_utf8_lossy(&translator_output.stderr)
    );

    let tick_limit = &spec["tick_limit"].as_i64().unwrap();
    let machine_output = Command::new("cargo")
        .arg("run")
        .arg("--bin")
        .arg("machine")
        .arg(&out_code_bin_path)
        .arg(&out_data_bin_path)
        .arg(&input_path)
        .arg(tick_limit.to_string())
        .output()
        .expect("Failed to run machine");

    assert!(
        machine_output.status.success(),
        "Machine failed: {}",
        String::from_utf8_lossy(&machine_output.stderr)
    );

    let stdout = String::from_utf8_lossy(&machine_output.stdout).into_owned();
    let log_limit = spec["log_limit"].as_u64().unwrap();

    let code_bin = test_utils::run_xxd(&out_code_bin_path);
    let data_bin = test_utils::run_xxd(&out_data_bin_path);
    let code_hex = test_utils::read_file_or_empty(&out_code_hex_path);
    let result_bin = test_utils::run_xxd(&result_data_path);
    let truncated_log = test_utils::truncate_log(&stdout, log_limit as usize);

    let out_code = spec["out_code"].as_str().unwrap();
    let out_data = spec["out_data"].as_str().unwrap();
    let out_code_hex = spec["out_code_hex"].as_str().unwrap();
    let result_data = spec["result_data"].as_str().unwrap();
    let out_log = spec["out_log"].as_str().unwrap();

    assert_eq!(code_hex.trim(), out_code_hex.trim(), "Hex dump mismatch");
    assert_eq!(code_bin.trim(), out_code.trim(), "Code binary mismatch");
    assert_eq!(data_bin.trim(), out_data.trim(), "Data binary mismatch");
    assert_eq!(
        result_bin.trim(),
        result_data.trim(),
        "Result data mismatch"
    );
    assert_eq!(truncated_log.trim(), out_log.trim(), "Log mismatch");
}
