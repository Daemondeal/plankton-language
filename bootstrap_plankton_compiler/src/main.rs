use std::{fs};

use bootstrap_plankton_compiler::{
    compiler::{Compiler, CompilerArgs, CompilerTarget, Source},
    PlanktonError, Span,
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
                        Err(e) => report_error(e, &compiler),
                    }
                }
                Err(e) => report_io_error(e),
            }
        }
        Err(str) => report_string_error(format!("{}", str)),
    }
}

fn write_result(_args: Args, result: String) {
    info!(target: "main", "Finished compiling! Result: {}", result);
}

fn load_files(source_paths: &[&str]) -> Result<Vec<Source>, std::io::Error> {
    let mut sources = vec![];

    for path in source_paths {
        let content = fs::read_to_string(path);
        match content {
            Ok(str) => sources.push(Source::new(path.to_string(), str)),
            Err(err) => return Err(err)
        }
    } 

    Ok(sources)
}

fn report_error(err: PlanktonError, compiler: &Compiler) {
    match err {
        PlanktonError::IOError(err) => report_io_error(err),
        PlanktonError::LexerError { message, span } => report_error_at_span(message, span, compiler),
        PlanktonError::ParserError { message, span } => report_error_at_span(message, span, compiler),
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

fn report_error_at_span(message: String, span: Span, compiler: &Compiler) {
    let source = compiler.get_source(span.file).unwrap().clone();

    report_message_at_span_pretty(&message, MessageType::Error, span, source)
}

pub enum MessageType {
    Error
}

impl MessageType {
    pub fn name(&self) -> &'static str {
        match self {
            MessageType::Error => "error"
        }
    } 
}

fn report_message_at_span_pretty(
    message: &str,
    message_type: MessageType,
    span: Span,
    source: Source
) {
    
    // TODO: Support more message types
    let _ = message_type;
    error!(target: &source.name, "{}", message);
    
    
    let mut current_position = 0;

    for line in source.content.lines() {
        if current_position + line.chars().count() > span.start {
            println!("{}", line);

            print!("{}", " ".repeat(span.start - current_position));
            println!("{}", "^".repeat(span.end - span.start));

            break;
        } else {
            println!("Cur_Pos: {}; Start: {}", current_position, span.start);
            println!("Before line: {}", line);
            current_position += line.chars().count();
        }
    }
}
