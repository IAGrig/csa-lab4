use std::fs;
use std::path::Path;
use std::process::Command;

use csa_lab4::test_utils;
use serde_yaml::{Mapping, Value};

fn main() {
    let test_files = fs::read_dir("tests/golden")
        .expect("Failed to read golden directory")
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            if path.extension()? == "yml" {
                Some(path)
            } else {
                None
            }
        });

    for test_file in test_files {
        println!("Updating: {}", test_file.display());

        let yaml_content = fs::read_to_string(&test_file).expect("Failed to read test file");
        let spec: Value = serde_yaml::from_str(&yaml_content).expect("Invalid YAML");

        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let temp_path = temp_dir.path();

        let source_path = temp_path.join("source.lsp");
        let out_path = temp_path.join("out");
        let input_path = temp_path.join("input");
        let out_code_bin_path = temp_path.join("out.bin");
        let out_code_hex_path = temp_path.join("out.bin.hex");
        let out_data_bin_path = temp_path.join("out.data.bin");
        let result_data_path = Path::new("result-data.bin");

        fs::write(&source_path, spec["in_source"].as_str().unwrap())
            .expect("Failed to write source");
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

        let mut new_spec = Mapping::new();
        new_spec.insert(
            Value::String("in_source".to_string()),
            spec["in_source"].clone(),
        );

        if let Some(stdin) = spec.get("in_stdin") {
            new_spec.insert(Value::String("in_stdin".to_string()), stdin.clone());
        }

        new_spec.insert(
            Value::String("tick_limit".to_string()),
            spec["tick_limit"].clone(),
        );

        new_spec.insert(
            Value::String("log_limit".to_string()),
            spec["log_limit"].clone(),
        );

        new_spec.insert(
            Value::String("out_code_hex".to_string()),
            Value::String(test_utils::read_file_or_empty(&out_code_hex_path)),
        );

        new_spec.insert(
            Value::String("out_code".to_string()),
            Value::String(test_utils::run_xxd(&temp_path.join("out.bin"))),
        );

        new_spec.insert(
            Value::String("out_data".to_string()),
            Value::String(test_utils::run_xxd(&temp_path.join("out.data.bin"))),
        );

        new_spec.insert(
            Value::String("result_data".to_string()),
            Value::String(test_utils::run_xxd(result_data_path)),
        );

        let log = String::from_utf8(machine_output.stdout).expect("Invalid UTF-8 in log");
        new_spec.insert(
            Value::String("out_log".to_string()),
            Value::String(
                test_utils::truncate_log(&log, spec["log_limit"].as_u64().unwrap() as usize)
                    .trim()
                    .to_string(),
            ),
        );

        fs::write(&test_file, serde_yaml::to_string(&new_spec).unwrap())
            .expect("Failed to write updated test file");
    }
}
