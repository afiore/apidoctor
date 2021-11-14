// use jsonschema;
use openapiv3::OpenAPI;

pub(crate) mod examples;

fn main() {
    let data = include_str!("../petstore.json");
    let openapi: OpenAPI = serde_json::from_str(data).expect("cannot parse");
    println!("{:?}", openapi);
}
