use std::{fs};

use bootstrap_plankton_compiler::{
    compiler::{Compiler, CompilerArgs, CompilerTarget},
    PlanktonError,
};
use log::{error, info};

#[allow(dead_code)]
struct Args {
    input_file_path: String,
    output_file_path: String,
    target: CompilerTarget,
}

fn parse_args() -> Result<Args, lexopt::Error> {
    use lexopt::prelude::*;

    let mut input_path = None;
    let mut output_path = "./out.out".to_string();
    let mut parser = lexopt::Parser::from_env();

    while let Some(arg) = parser.next()? {
        match arg {
            Short('o') | Long("output") => {
                output_path = parser.value()?.into_string()?;
            }
            Value(path) if input_path.is_none() => {
                input_path = Some(path.into_string()?);
            }
            _ => return Err(arg.unexpected()),
        }
    }

    Ok(Args {
        input_file_path: input_path.ok_or("Missing input file")?,
        output_file_path: output_path,
        target: CompilerTarget::MIPS, // TODO: Allow changing targets
    })
}

fn main() {
    pretty_env_logger::init();

    // TODO: Fix this spaghetti
    match parse_args() {
        Ok(args) => {
            let sources_res = load_files(&[&args.input_file_path]);

            match sources_res {
                Ok(sources) => {
                    let mut compiler = Compiler::new(CompilerArgs {
                        input_sources: sources,
                    });

                    let res = compiler.compile_target(args.target);
                    match res {
                        Ok(result) => write_result(args, result),
                        Err(e) => report_error(e),
                    }
                }
                Err(e) => report_error(e),
            }
        }
        Err(str) => report_string_error(format!("{}", str)),
    }
}

fn write_result(_args: Args, result: String) {
    info!(target: "main", "Finished compiling! Result: {}", result);
}

fn load_files(sources: &[&str]) -> Result<Vec<String>, PlanktonError> {
    // FIXME: There must be a better way
    let res = sources
        .iter()
        .map(fs::read_to_string)
        .collect::<Result<Vec<_>, std::io::Error>>();

    match res {
        Ok(strings) => Ok(strings),
        Err(err) => Err(err.into()) 
    }
}

fn report_error(err: PlanktonError) {
    match err {
        PlanktonError::IOError(err) => report_io_error(err),
    }
}

// TODO: Use a better method of reporting io errors.
fn report_io_error(err: std::io::Error) {
    panic!("{}", err)
}

// TODO: Use a better method of reporting errors.
fn report_string_error(err: String) {
    error!("{}", err);
}
