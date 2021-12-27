use std::{convert::Infallible, env, error::Error, fmt::Display, path::PathBuf, str::FromStr};

use examples::AppError;
use futures::executor;
use openapi::{LintingOutcome, OperationId};
use structopt::StructOpt;
use thiserror::Error;

use crate::openapi::LintingIssues;

mod examples;
mod openapi;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Tag(String);

impl Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Tag {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Tag(s.to_owned()))
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

    match outcome {
        LintingOutcome::OperationNotFound(operation_id) => {
            Err(AppError::OperationNotFound(operation_id).into())
        }

        LintingOutcome::AllGood(stats) => Ok(println!("{}", stats)),
        LintingOutcome::IssuesFound {
            stats,
            operation_linting_issues,
        } => {
            println!("{}", stats);
            for (i, (context, LintingIssues { issues })) in
                operation_linting_issues.iter().enumerate()
            {
                let s = if issues.len() > 1 { "s" } else { "" };
                let tags: Vec<String> = context.tags.iter().map(ToString::to_string).collect();

                println!(
                    "{:0>3}. {:<40} tags: {:<35} {:>2} issue{}:\n",
                    i + 1,
                    format!("{}", context.id),
                    tags.join(", "),
                    issues.len(),
                    s,
                );

                for err in issues {
                    println!("* {}", err);
                }
            }
            Err(CmdError::ValidationFailed.into())
        }
    }
}
