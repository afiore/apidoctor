// use jsonschema;
use openapiv3::OpenAPI;

mod examples;

fn main() {
    let data = include_str!("../petstore.json");
    let spec: OpenAPI = serde_json::from_str(data).expect("cannot parse");
    if let Err(errors) = examples::validate_from_spec(&spec) {
        eprint!("Validation failed: {:?}", errors);
    } else {
        eprint!("LGTM!")
    }
}
