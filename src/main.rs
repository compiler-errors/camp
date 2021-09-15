use camino::Utf8PathBuf;
use camp_driver::{parse_stage, CampDb, CampsiteDecl, DriverError};
use codespan_derive::IntoDiagnostic;
use log::LevelFilter;
use structopt::StructOpt;
use termcolor::{ColorChoice, StandardStream};

#[derive(StructOpt, Debug)]
struct Args {
    /// Verbosity level. 0 = Errors, 1 = Info, 2 = Debug, 3+ = Trace
    #[structopt(short, parse(from_occurrences = log_level))]
    verbose: LevelFilter,
    /// Exclude color from terminal output
    #[structopt(long)]
    no_color: bool,
    /// Compile mode
    #[structopt(subcommand)]
    mode: Mode,
}

fn log_level(verbose: u64) -> LevelFilter {
    match verbose {
        0 => LevelFilter::Error,
        1 => LevelFilter::Info,
        2 => LevelFilter::Debug,
        _ => LevelFilter::Trace,
    }
}

#[derive(StructOpt, Debug)]
enum Mode {
    Lex {
        #[structopt(parse(from_str = Utf8PathBuf::from))]
        file: Utf8PathBuf,
    },
    /// Parse the site module and declared submodules
    Parse {
        /// The site module of the program
        #[structopt()]
        campsite_file: CampsiteDecl,
    },
    /// Lower and typecheck the site module, any additional libraries, and
    /// stdlib
    Verify {
        /// The main site of the program, formatted like
        /// name=path/to/project/site/main.camp
        #[structopt()]
        site: CampsiteDecl,
        #[structopt(long, short = "L")]
        libs: Vec<CampsiteDecl>,
    },
    /// Compile the site module, any additional libraries, and stdlib into a
    /// binary
    Compile {
        /// The main site of the program, formatted like
        /// name=path/to/project/site/main.camp
        #[structopt()]
        site: CampsiteDecl,
        #[structopt(long, short = "L")]
        libs: Vec<CampsiteDecl>,
        /// Output path of the executable generated by successful compilation
        #[structopt(parse(from_str = Utf8PathBuf::from), default_value = "camp.out")]
        output: Utf8PathBuf,
    },
}

impl Mode {
    /// Turn a compiler mode args enum into a displayable string
    fn name(&self) -> &'static str {
        match self {
            Mode::Lex { .. } => "lex",
            Mode::Parse { .. } => "parse",
            Mode::Verify { .. } => "verify",
            Mode::Compile { .. } => "compile",
        }
    }
}

fn main() {
    // Parse arguments, bailing if we have mistyped
    let args = Args::from_args();

    // Set up logging with the declared verbosity (see `crate::log_level`)
    env_logger::init();

    let mut db = CampDb::default();

    let output = match args.mode {
        Mode::Parse { campsite_file } => parse_stage(&mut db, campsite_file),
        mode => Err(DriverError::UnsupportedMode(mode.name())),
    };

    std::process::exit(match output {
        Ok(()) => 0,
        Err(e) => {
            let color = if args.no_color {
                ColorChoice::Never
            } else {
                ColorChoice::Auto
            };
            let mut writer = StandardStream::stderr(color);
            let config = codespan_reporting::term::Config::default();
            codespan_reporting::term::emit(&mut writer, &config, &db, &e.into_diagnostic())
                .unwrap();
            // Exit with code 1
            1
        },
    });
}
