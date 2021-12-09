use std::{env, error::Error, path::PathBuf};

use openapiv3::OpenAPI;
use structopt::StructOpt;
use thiserror::Error;

mod examples;
mod openapi;

#[derive(Debug, StructOpt)]
#[structopt(name = "apidoctor", about = "An API spec linter")]
struct Cmd {
    #[structopt(parse(from_os_str))]
    spec: PathBuf,
}

#[derive(Error, Debug)]
pub enum CmdError {
    #[error("validation failed!")]
    ValidationFailed,
}

fn main() -> Result<(), Box<dyn Error>> {
    let cmd = Cmd::from_iter(env::args());
    let spec = std::fs::read_to_string(&cmd.spec)?;
    let spec: OpenAPI = serde_json::from_str(&spec)?;
    let outcome = openapi::validate_from_spec(&spec);

    println!("stats: {:?}", outcome.stats);

    if let Err(errors) = outcome.result {
        for (operation_id, errors) in errors {
            println!("[{}]\n", operation_id,);
            for err in errors {
                println!("{}", err);
            }
        }
        Err(CmdError::ValidationFailed.into())
    } else {
        Ok(())
    }
}
