use camino::Utf8PathBuf;

use camp_util::ui_test as framework;

#[test]
fn ui_tests() -> framework::Result<()> {
    let pass_tests = framework::get_ui_test_files("parser/pass")?;
    let fail_tests = framework::get_ui_test_files("parser/fail")?;

    let mut failing = vec![];

    for test in pass_tests {
        if !parse_file(&test, true)? {
            failing.push(test.path);
        }
    }

    for test in fail_tests {
        if !parse_file(&test, false)? {
            failing.push(test.path);
        }
    }

    framework::flush()?;
    framework::report_failing_tests(failing)
}

fn parse_file(test: &framework::UITest, expected_status: bool) -> framework::Result<bool> {
    let (status, stdout, stderr) =
        framework::cargo_run(&["parse", &format!("test={}", test.path)])?;

    framework::compare_test_data(test, expected_status, status, stdout, stderr)
}

#[test]
fn std_test() -> framework::Result<()> {
    let (status, _stdout, stderr) = framework::cargo_run(&["parse", "std=std/lib.camp"])?;

    // Fake test data, because we expect std to compile normally
    let std_test = framework::UITest {
        display: "Standard Library".to_string(),
        path: Utf8PathBuf::from("ui_test/std"),
        stdout: Utf8PathBuf::from("ui_test/std.out"),
        stderr: Utf8PathBuf::from("ui_test/std.err"),
    };

    if !framework::compare_test_data(&std_test, true, status, "".to_string(), stderr)? {
        framework::report_failing_tests(vec![std_test.path])?;
    }

    framework::flush()?;
    Ok(())
}
