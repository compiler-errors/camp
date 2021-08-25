use std::{convert::TryInto, io::Write, process::Command, sync::Mutex};

use camino::{Utf8Path, Utf8PathBuf};
use lazy_static::lazy_static;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error + 'static>>;

const DIVIDER: &str = "----------------------------------------";

lazy_static! {
    static ref TERM: Mutex<StandardStream> = Mutex::new(StandardStream::stderr(ColorChoice::Auto));
    static ref BOLD: ColorSpec = {
        let mut color = ColorSpec::new();
        color.set_bold(true);
        color
    };
    static ref YELLOW: ColorSpec = {
        let mut color = ColorSpec::new();
        color.set_fg(Some(Color::Yellow));
        color
    };
    static ref RED: ColorSpec = {
        let mut color = ColorSpec::new();
        color.set_fg(Some(Color::Red));
        color
    };
    static ref BOLD_RED: ColorSpec = {
        let mut color = ColorSpec::new();
        color.set_fg(Some(Color::Red));
        color.set_bold(true);
        color
    };
    static ref GREEN: ColorSpec = {
        let mut color = ColorSpec::new();
        color.set_fg(Some(Color::Green));
        color
    };
    static ref BLUE: ColorSpec = {
        let mut color = ColorSpec::new();
        color.set_fg(Some(Color::Blue));
        color
    };
    static ref BOLD_BLUE: ColorSpec = {
        let mut color = ColorSpec::new();
        color.set_bold(true);
        color.set_fg(Some(Color::Blue));
        color
    };
}

pub fn flush() -> Result<()> {
    TERM.lock()?.flush()?;
    Ok(())
}
pub struct UITest {
    pub display: String,
    pub path: Utf8PathBuf,
    pub stdout: Utf8PathBuf,
    pub stderr: Utf8PathBuf,
}

pub fn get_ui_test_files(dir_name: &str) -> Result<Vec<UITest>> {
    let dir_path: Utf8PathBuf = Utf8PathBuf::from(format!("ui_test/{}/", dir_name))
        .canonicalize()
        .expect("Expected to be able to canonicalize path")
        .try_into()
        .expect("Expected canonicalized path to be utf-8");
    assert!(dir_path.is_dir(), "Expected {} to be a directory", dir_path);

    let mut tests = vec![];

    for entry in std::fs::read_dir(&dir_path)? {
        let entry = entry?;
        let path: Utf8PathBuf = entry.path().try_into()?;

        if path.extension() == Some("camp") {
            let mut stdout = path.clone();
            stdout.set_extension("out");
            let mut stderr = path.clone();
            stderr.set_extension("err");

            tests.push(UITest {
                display: format!(
                    "ui_test/{}/{}",
                    dir_name,
                    path.file_name().expect("Expected file name")
                ),
                path,
                stdout,
                stderr,
            });
        }
    }

    Ok(tests)
}

pub fn cargo_run(args: &[&str]) -> Result<(bool, String, String)> {
    let output = Command::new("cargo")
        .arg("run")
        .arg("--quiet")
        .arg("--features=ignore_spans")
        .arg("--")
        .arg("--no-color")
        .args(args)
        .output()?;

    Ok((
        output.status.success(),
        String::from_utf8(output.stdout)?,
        String::from_utf8(output.stderr)?,
    ))
}

pub fn compare_test_data(
    test: &UITest,
    expected_status: bool,
    status: bool,
    stdout: String,
    stderr: String,
) -> Result<bool> {
    let dirname = test
        .path
        .parent()
        .expect("Expected parent dir for test file")
        .as_str();

    let status_match = expected_status == status;

    let stdout = sanitize_dirname(&stdout, dirname);
    let expected_stdout = read_or_empty(&test.stdout)?;
    let stdout_match = stdout == expected_stdout;

    let stderr = sanitize_dirname(&stderr, dirname);
    let expected_stderr = read_or_empty(&test.stderr)?;
    let stderr_match = stderr == expected_stderr;

    let pass = status_match && stdout_match && stderr_match;

    let mut term = TERM.lock()?;
    write!(*term, "Testing ")?;
    term.set_color(&*BOLD)?;
    write!(term, "{}", test.display)?;
    term.reset()?;
    write!(term, "... ")?;

    if pass {
        term.set_color(&*GREEN)?;
        writeln!(term, "pass")?;
    } else {
        term.set_color(&*BOLD_RED)?;
        writeln!(term, "FAIL")?;
    }

    term.reset()?;

    if !status_match {
        writeln!(term, "")?;
        write!(term, "Exit status mismatch. ")?;
        if status && !expected_status {
            term.set_color(&*YELLOW)?;
            write!(term, "Expected test to fail, but it passed.")?;
        } else if !status && expected_status {
            term.set_color(&*RED)?;
            write!(term, "Expected test to pass, but it failed.")?;
        }
        term.reset()?;
    }

    if !stdout_match {
        writeln!(term, "")?;
        writeln!(term, "Output (stdout) mismatch.")?;
        term.set_color(&*BOLD_BLUE)?;
        writeln!(term, "EXPECTED:")?;
        term.set_color(&*BLUE)?;
        writeln!(term, "{}", DIVIDER)?;
        write!(term, "{}", expected_stdout)?;
        writeln!(term, "{}", DIVIDER)?;
        term.reset()?;
        writeln!(term, "")?;
        term.set_color(&*BOLD_RED)?;
        writeln!(term, "ACTUAL:")?;
        term.set_color(&*RED)?;
        writeln!(term, "{}", DIVIDER)?;
        write!(term, "{}", stdout)?;
        writeln!(term, "{}", DIVIDER)?;
        term.reset()?;
        fixup_test_if_needed(&test.stdout, stdout)?;
    }

    if !stderr_match {
        writeln!(term, "")?;
        writeln!(term, "Error (stderr) mismatch.")?;
        term.set_color(&*BOLD_BLUE)?;
        writeln!(term, "EXPECTED:")?;
        term.set_color(&*BLUE)?;
        writeln!(term, "{}", DIVIDER)?;
        write!(term, "{}", expected_stderr)?;
        writeln!(term, "{}", DIVIDER)?;
        term.reset()?;
        writeln!(term, "")?;
        term.set_color(&*BOLD_RED)?;
        writeln!(term, "ACTUAL:")?;
        term.set_color(&*RED)?;
        writeln!(term, "{}", DIVIDER)?;
        write!(term, "{}", stderr)?;
        writeln!(term, "{}", DIVIDER)?;
        term.reset()?;
        fixup_test_if_needed(&test.stderr, stderr)?;
    }

    if !pass {
        writeln!(term, "")?;
    }

    Ok(pass)
}

fn sanitize_dirname(input: &str, dirname: &str) -> String {
    input.replace(dirname, "$DIR")
}

fn read_or_empty(file: &Utf8Path) -> Result<String> {
    if !file.exists() {
        Ok("".to_string())
    } else {
        Ok(std::fs::read_to_string(&file)?)
    }
}

#[derive(Debug)]
struct FailingTestsError {
    failing_tests: Vec<Utf8PathBuf>,
}

impl std::error::Error for FailingTestsError {}

impl std::fmt::Display for FailingTestsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Failing tests:")?;

        for test in &self.failing_tests {
            writeln!(f, "- {}", test)?;
        }

        Ok(())
    }
}

pub fn report_failing_tests(failing_tests: Vec<Utf8PathBuf>) -> Result<()> {
    if failing_tests.is_empty() {
        Ok(())
    } else {
        Err(FailingTestsError { failing_tests }.into())
    }
}

fn fixup_test_if_needed(path: &Utf8Path, contents: String) -> Result<()> {
    if std::env::var("FIXUP_TESTS").map_or(false, |var| &var == "1") {
        debug!("Fixing up test: {}", path);

        if contents.is_empty() {
            if path.exists() {
                std::fs::remove_file(path)?;
            }
        } else {
            std::fs::write(path, contents)?;
        }
    }

    Ok(())
}
