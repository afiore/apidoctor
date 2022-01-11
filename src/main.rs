use std::{convert::Infallible, env, fmt::Display, io, path::PathBuf, str::FromStr};

use futures::executor;
use main_error::MainError;
use openapi::operations::OperationId;
use structopt::StructOpt;

use crate::openapi::linting::{lint, LintingOutcome};

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

#[derive(thiserror::Error, Debug)]
enum AppError {
    #[error(
        "Could not deserialise the supplied spec file. Is it a valid, json-encoded, OpenAPI spec?"
    )]
    UnparsableJsonSpec(#[from] serde_json::Error),
    #[error(
        "Could not deserialise the supplied spec file. Is it a valid, yaml-encoded, OpenAPI spec?"
    )]
    UnparsableYamlSpec(#[from] serde_yaml::Error),
    #[error("Could not find spec file")]
    SpecFileNotFound(#[from] io::Error),
    #[error("Could not find operation_id {}", 0)]
    OperationNotFound(OperationId),
    #[error("Linting failed")]
    LintingFailed,
}

#[derive(Debug, StructOpt)]
#[structopt(name = "apidoctor", about = "An API spec linter")]
enum Cmd {
    Lint {
        /// path to JSON or YAML spec
        #[structopt(parse(from_os_str))]
        spec: PathBuf,
        /// Filter issue by the given operation id
        #[structopt(short, long)]
        operation_id: Option<OperationId>,
        /// Filter issue by the given set of tags
        #[structopt(short, long)]
        tags: Vec<Tag>,
    },
}

fn main() -> Result<(), MainError> {
    let cmd = Cmd::from_iter(env::args());
    match cmd {
        Cmd::Lint {
            spec,
            operation_id,
            tags,
        } => {
            let spec = openapi::spec_from_file(&spec)?;
            let outcome = executor::block_on(lint(&spec, tags, operation_id));

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
                    for (i, (context, issues)) in operation_linting_issues.iter().enumerate() {
                        let s = if issues.len() > 1 { "s" } else { "" };
                        let tags: Vec<String> =
                            context.tags.iter().map(ToString::to_string).collect();

                        println!(
                            "{:0>3}. {:<40} tags: {:<35} {:>2} issue{}:\n",
                            i + 1,
                            format!("{}", context.id),
                            tags.join(", "),
                            issues.len(),
                            s,
                        );

                        for err in issues {
                            println!("* {}\n", err);
                        }
                    }
                    Err(AppError::LintingFailed.into())
                }
            }
        }
    }
}
