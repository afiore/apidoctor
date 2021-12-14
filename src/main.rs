use std::{
    env,
    error::Error,
    fs::File,
    io::BufReader,
    path::{Path, PathBuf},
};

use futures::executor;
use openapiv3::OpenAPI;
use structopt::StructOpt;
use thiserror::Error;

mod examples;
mod openapi;

#[derive(Debug, StructOpt)]
#[structopt(name = "apidoctor", about = "An API spec linter")]
struct Cmd {
    /// path to JSON or YAML spec
    #[structopt(parse(from_os_str))]
    spec: PathBuf,
}

#[derive(Error, Debug)]
pub enum CmdError {
    #[error("validation failed!")]
    ValidationFailed,
}

fn spec_from_file<P: AsRef<Path>>(path: P) -> Result<OpenAPI, Box<dyn Error>> {
    let file = File::open(path.as_ref())?;
    let reader = BufReader::new(file);
    let extension = path.as_ref().extension().and_then(|ext| ext.to_str());

    if let Some("yml") | Some("yaml") = extension {
        serde_yaml::from_reader(reader).map_err(Into::into)
    } else {
        serde_json::from_reader(reader).map_err(Into::into)
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let cmd = Cmd::from_iter(env::args());
    let spec: OpenAPI = spec_from_file(&cmd.spec)?;
    let outcome = executor::block_on(openapi::validate_from_spec(&spec));

    println!("{}", outcome.stats);

    if let Err(errors) = outcome.result {
        for (i, (operation_id, errors)) in errors.iter().enumerate() {
            let s = if errors.len() > 0 { "s" } else { "" };
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
