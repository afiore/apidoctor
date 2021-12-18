use std::{env, error::Error, fmt::Display, path::PathBuf, str::FromStr};

use futures::executor;
use openapi::OperationId;
use structopt::StructOpt;
use thiserror::Error;

mod examples;
mod openapi;

#[derive(Debug)]
struct Tag(String);

#[derive(Debug)]
struct InvalidTagError(pub String);

impl Display for InvalidTagError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}' contains non-alphanumeric characters", self.0)
    }
}

impl FromStr for Tag {
    type Err = InvalidTagError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s1 = s.to_owned();
        if s.chars().all(|c| c.is_alphanumeric()) {
            Ok(Tag(s1))
        } else {
            Err(InvalidTagError(s1))
        }
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "apidoctor", about = "An API spec linter")]
struct Cmd {
    /// path to JSON or YAML spec
    #[structopt(parse(from_os_str))]
    spec: PathBuf,
    /// Filter issue by the given operation id
    #[structopt(short, long)]
    operation_id: Option<OperationId>,
    /// Filter issue by the given set of tags
    #[structopt(short, long)]
    tags: Vec<Tag>,
}

#[derive(Error, Debug)]
pub enum CmdError {
    #[error("validation failed!")]
    ValidationFailed,
}

fn main() -> Result<(), Box<dyn Error>> {
    let cmd = Cmd::from_iter(env::args());
    let spec = openapi::spec_from_file(&cmd.spec)?;
    let outcome = executor::block_on(openapi::lint(&spec, cmd.tags, cmd.operation_id));

    println!("{}", outcome.stats);

    if let Err(errors) = outcome.result {
        for (i, (operation_id, errors)) in errors.iter().enumerate() {
            let s = if errors.len() > 1 { "s" } else { "" };
            println!(
                "{:0>3}. {:<60} {:>2} issue{}:\n",
                i + 1,
                format!("{}", operation_id),
                errors.len(),
                s,
            );

            for err in errors {
                println!("* {}", err);
            }
        }
        Err(CmdError::ValidationFailed.into())
    } else {
        Ok(())
    }
}
