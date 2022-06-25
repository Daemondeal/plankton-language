use std::fs;

use bootstrap_plankton_compiler::{
    compiler::{Compiler, CompilerArgs, CompilerTarget, Source},
    PlanktonError, Span,
};
use log::{error, info, LevelFilter};
use simplelog::{ColorChoice, Config, TermLogger, TerminalMode};

#[allow(dead_code)]
struct Args {
    input_file_path: String,
    output_file_path: String,
    target: CompilerTarget,
    json_errors: bool,
    verbosity: LevelFilter,
}

fn parse_args() -> Result<Args, lexopt::Error> {
    use lexopt::prelude::*;

    let mut input_path = None;
    let mut output_path = "./out.out".to_string();
    let mut parser = lexopt::Parser::from_env();
    let mut json_errors = false;
    let mut verbosity = LevelFilter::Info;

    while let Some(arg) = parser.next()? {
        match arg {
            Short('o') | Long("output") => output_path = parser.value()?.into_string()?,

            Value(path) if input_path.is_none() => input_path = Some(path.into_string()?),

            Short('j') | Long("json") => json_errors = true,

            Short('q') | Long("quiet") => verbosity = LevelFilter::Off,
            Long("trace") => verbosity = LevelFilter::Trace,

            _ => return Err(arg.unexpected()),
        }
    }

    Ok(Args {
        input_file_path: input_path.ok_or("Missing input file")?,
        output_file_path: output_path,
        target: CompilerTarget::MIPS, // TODO: Allow changing targets
        json_errors,
        verbosity,
    })
}

fn main() {
    // TODO: Fix this spaghetti
    match parse_args() {
        Ok(args) => {
            let sources_res = load_files(&[&args.input_file_path]);

            // FIXME: With the current implementation, if parsing arguments fails nothing will get output,
            //        because we use an uninitializated logger. Fix this.
            TermLogger::init(
                args.verbosity,
                Config::default(),
                TerminalMode::Mixed,
                ColorChoice::Auto,
            )
            .unwrap();

            match sources_res {
                Ok(sources) => {
                    let mut compiler = Compiler::new(CompilerArgs {
                        input_sources: sources,
                    });

                    let res = compiler.compile_target(args.target);
                    match res {
                        Ok(result) => write_result(args, result),
                        Err(e) => report_all_errors(e, &compiler, args.json_errors),
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
            Err(err) => return Err(err),
        }
    }

    Ok(sources)
}

fn report_all_errors(errs: Vec<PlanktonError>, compiler: &Compiler, json: bool) {
    if json {
        println!("[");
    }

    for err in errs {
        report_error(err, compiler, json);
        if json {
            println!(",");
        }
    }

    if json {
        println!("]");
    }
}

fn report_error(err: PlanktonError, compiler: &Compiler, json: bool) {
    match err {
        PlanktonError::IOError(err) => report_io_error(err),
        PlanktonError::LexerError { message, span } => {
            report_error_at_span(message, span, compiler, json)
        }
        PlanktonError::ParserError { message, span } => {
            report_error_at_span(message, span, compiler, json)
        }
        PlanktonError::ParserErrrorWithoutSpan(err) => report_string_error(err),
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

fn report_error_at_span(message: String, span: Span, compiler: &Compiler, json: bool) {
    let source = compiler.get_source(span.file).unwrap().clone();

    if json {
        report_error_at_span_json(&message, MessageType::Error, span, source);
    } else {
        report_message_at_span_pretty(&message, MessageType::Error, span, source)
    }
}

pub enum MessageType {
    Error,
}

impl MessageType {
    pub fn name(&self) -> &'static str {
        match self {
            MessageType::Error => "error",
        }
    }
}

fn report_error_at_span_json(message: &str, message_type: MessageType, span: Span, source: Source) {
    // TODO: Allow more message types
    let _ = message_type;

    print!(
        "{{ \"error\": \"{}\", \"span\": {{\"start\": {}, \"end\": {}, \"file\": {}}} }}",
        message, span.start, span.end, source.name
    );
}

fn find_line_breakpoints(chars: Vec<char>) -> Vec<usize> {
    let mut res = vec![0];
    let mut pos = 0;

    for char in chars {
        pos += 1;
        if char == '\n' {
            res.push(pos);
        }
    }

    res
}

fn report_message_at_span_pretty(
    message: &str,
    message_type: MessageType,
    span: Span,
    source: Source,
) {
    // TODO: Support more message types
    let _ = message_type;
    error!(target: &source.name, "{}: {}", &source.name, message);

    let chars = source.content.chars().collect::<Vec<_>>();
    let line_breakpoints = find_line_breakpoints(chars);

    for (i, breakpoint) in line_breakpoints.iter().enumerate().skip(1) {
        if breakpoint > &span.start {
            let last_pos = line_breakpoints[i - 1];
            // That -1 is because we don't want to print the '\n'
            let slice = &source.content[last_pos..*breakpoint - 1];

            error!("{}", slice);

            error!(
                "{}{}",
                " ".repeat(span.start - last_pos),
                "^".repeat(span.end - span.start)
            );

            break;
        }
    }
}
