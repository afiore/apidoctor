use std::fmt::Display;
use std::io;

use indexmap::IndexMap;
use jsonschema::JSONSchema;
use nonempty::NonEmpty;
use openapiv3::MediaType;
use openapiv3::OpenAPI;
use openapiv3::Operation;
use openapiv3::PathItem;
use openapiv3::ReferenceOr;
use openapiv3::RequestBody;
use openapiv3::Response;
use openapiv3::Schema;
use openapiv3::StatusCode;
use serde_json::Value;
use thiserror::Error;

#[derive(Debug)]
pub(crate) struct ValidationError {
    message: String,
}

impl<'a> From<jsonschema::ValidationError<'a>> for ValidationError {
    fn from(err: jsonschema::ValidationError<'a>) -> Self {
        ValidationError {
            message: format!("{}", err),
        }
    }
}

#[derive(Error, Debug)]
pub(crate) enum AppError {
    #[error(
        "Could not deserialise the supplied spec file. Is it a valid, json-encoded, OpenAPI spec?"
    )]
    DeserializeationError(#[from] serde_json::Error),
    #[error("Could not find spec file")]
    SpecFileNotFound(#[from] io::Error),
    #[error("Schema validation failed for some examples")]
    ValidationFailed(NonEmpty<ValidationError>),
    #[error("Unexpected error. Couldn't compile JSON schema")]
    InvalidSchema,
}

#[derive(Debug)]
pub struct ExampleError {
    is_request: bool,
    example: Value,
    schema: Value,
    error: AppError,
}

impl Display for ExampleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let snippet = serde_json::to_string_pretty(&self.example)
            .expect("failed to serialise example to JSON");

        let errors = match &self.error {
            AppError::ValidationFailed(validation_errs) => {
                let mut s = String::new();
                for err in validation_errs {
                    s.push_str(&format!(" - {}\n", err.message))
                }
                s
            }

            _ => format!("{}", &self.error),
        };

        let request_response = if self.is_request {
            "request"
        } else {
            "response"
        };

        let schema =
            serde_json::to_string_pretty(&self.schema).expect("failed to serialise schema to JSON");

        write!(
            f,
            "Sample {} payload:\n\n{}\n\nValidation Errors:\n\n{}\n\nJSON Schema:\n\n{}",
            request_response, snippet, errors, schema
        )
    }
}

pub(crate) type ErrorReport = IndexMap<OperationId, NonEmpty<ExampleError>>;

//TODO: delete this
#[derive(Debug)]
struct ExampleWithSchema {
    example: Value,
    schema: Value,
}
impl ExampleWithSchema {
    fn into_example_payload(self, is_request: bool) -> ExamplePayload {
        ExamplePayload {
            is_request,
            example: self.example,
            schema: self.schema,
        }
    }
}

#[derive(Debug)]
struct ExamplePayload {
    is_request: bool,
    example: Value,
    schema: Value,
}

fn to_reference(value: &Value) -> Option<&str> {
    value.as_object().and_then(|props| {
        props
            .get("$ref")
            .and_then(|r| r.as_str())
            .filter(|_| props.len() == 1)
    })
}

fn expand_schema_refs(schema: &mut Value, definitions: &IndexMap<String, Value>) {
    match schema {
        Value::Array(items) => {
            for item in items {
                expand_schema_refs(item, definitions);
            }
        }
        //TODO: is there a way to avoid calling to_reference twice?
        obj if to_reference(schema).is_some() => {
            if let Some(reference) = to_reference(obj) {
                let reference = strip_component_prefix(reference);
                let definition = definitions
                    .get(&reference)
                    .expect(&format!("expected to find reference: {}", &reference));

                //a reference can contain other references
                let mut definition = definition.clone();
                expand_schema_refs(&mut definition, definitions);
                *obj = definition;
            }
        }
        Value::Object(props) => {
            for (_, value) in props {
                expand_schema_refs(value, definitions);
            }
        }
        _ => (),
    }
}

impl ExamplePayload {
    fn validate(&self, definitions: &IndexMap<String, Value>) -> Result<(), ExampleError> {
        let mut schema = self.schema.clone();
        expand_schema_refs(&mut schema, definitions);
        let compiled_schema = JSONSchema::compile(&schema).or_else(|_err| {
            Err(ExampleError {
                is_request: self.is_request,
                example: self.example.clone(),
                schema: schema.clone(),
                error: AppError::InvalidSchema,
            })
        })?;

        compiled_schema.validate(&self.example).map_err(|errors| {
            let error = AppError::ValidationFailed(
                NonEmpty::from_vec(
                    errors
                        .map(ValidationError::from)
                        .collect::<Vec<ValidationError>>(),
                )
                .expect("non-empty vec of ValidationError expected"),
            );
            ExampleError {
                is_request: self.is_request,
                example: self.example.clone(),
                schema: self.schema.clone(),
                error,
            }
        })
    }

    fn from_media_types(
        media_types: &IndexMap<String, MediaType>,
        schemas: &IndexMap<String, Schema>,
        is_request: bool,
    ) -> Vec<ExamplePayload> {
        let mut examples = Vec::new();
        for (media_type, media_type_response) in media_types {
            if is_json_media_type(media_type) {
                if let Some(example) = Option::from(WithSchemas {
                    schemas,
                    value: media_type_response.clone(), //avoid clone?
                })
                .map(|example: ExampleWithSchema| example.into_example_payload(is_request))
                {
                    examples.push(example);
                }
            }
        }
        examples
    }
}

struct WithSchemas<'s, T> {
    schemas: &'s IndexMap<String, Schema>,
    value: T,
}
fn extract_or_resolve<'a, T>(
    all_by_ref: &'a IndexMap<String, T>,
    ref_or_value: &'a ReferenceOr<T>,
) -> Option<&'a T> {
    extract_or_apply(all_by_ref, ref_or_value, |t| t)
}

fn strip_component_prefix(s: &str) -> String {
    let regex = regex::Regex::new("^#/components/[^/]*/").unwrap();
    regex.replace_all(s, "").as_ref().to_owned()
}

fn extract_or_apply<'a, T, S, F>(
    all_by_ref: &'a IndexMap<String, S>,
    ref_or_value: &'a ReferenceOr<T>,
    f: F,
) -> Option<&'a S>
where
    F: Fn(&'a T) -> &'a S,
{
    match ref_or_value {
        ReferenceOr::Item(t) => Some(f(t)),
        ReferenceOr::Reference { reference } => {
            let reference = strip_component_prefix(reference);
            all_by_ref.get(&reference)
        }
    }
}

impl<'s> From<WithSchemas<'s, MediaType>> for Option<ExampleWithSchema> {
    fn from(with_schemas: WithSchemas<MediaType>) -> Self {
        let WithSchemas { schemas, value } = with_schemas;
        value.example.and_then(|example| {
            value.schema.and_then(|schema_or_ref| {
                extract_or_resolve(&schemas, &schema_or_ref).map(|schema| {
                    let schema = serde_json::to_value(schema)
                        .expect(&format!("Cannot serialise as JSON: {:?}", schema));
                    ExampleWithSchema { example, schema }
                })
            })
        })
    }
}

#[derive(PartialEq, Clone, Debug, Hash, Eq)]
enum HttpMethod {
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
    HEAD,
    OPTIONS,
    TRACE,
}

impl Display for HttpMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = format!("{:?}", self);
        write!(f, "{}", s.to_lowercase())
    }
}

impl HttpMethod {
    fn all() -> Vec<HttpMethod> {
        vec![
            HttpMethod::GET,
            HttpMethod::POST,
            HttpMethod::PUT,
            HttpMethod::DELETE,
            HttpMethod::PATCH,
            HttpMethod::HEAD,
            HttpMethod::OPTIONS,
            HttpMethod::TRACE,
        ]
    }
}
fn operation_for<'s>(
    method: &HttpMethod,
    path_item: &'s PathItem,
) -> Option<&'s openapiv3::Operation> {
    match method {
        HttpMethod::GET => path_item.get.as_ref(),
        HttpMethod::POST => path_item.post.as_ref(),
        HttpMethod::PUT => path_item.put.as_ref(),
        HttpMethod::DELETE => path_item.delete.as_ref(),
        HttpMethod::HEAD => path_item.head.as_ref(),
        HttpMethod::PATCH => path_item.patch.as_ref(),
        HttpMethod::OPTIONS => path_item.options.as_ref(),
        HttpMethod::TRACE => path_item.trace.as_ref(),
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct OperationId(String);

impl Display for OperationId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TryFrom<&Operation> for OperationId {
    type Error = ();

    fn try_from(op: &Operation) -> Result<Self, Self::Error> {
        op.operation_id.clone().map(|id| OperationId(id)).ok_or(())
    }
}

fn capitalize(s: &str) -> String {
    let mut done = false;
    s.chars()
        .map(|c| {
            if done {
                c
            } else {
                done = true;
                c.to_ascii_uppercase()
            }
        })
        .collect()
}
fn camelize(path: &str) -> String {
    path.split_terminator('/').map(|s| capitalize(s)).collect()
}

impl OperationId {
    fn for_method_and_path(method: &HttpMethod, path: &str) -> Self {
        let id = format!("{}{}", method, camelize(path));
        OperationId(id)
    }
}

#[derive(Debug)]
struct Components {
    schemas: IndexMap<String, Schema>,
    responses: IndexMap<String, Response>,
    requests: IndexMap<String, RequestBody>,
}

#[derive(Debug)]
struct OperationExamples {
    operation_id: OperationId,
    examples: NonEmpty<ExamplePayload>,
}
impl OperationExamples {
    fn validate(&self, definitions: &IndexMap<String, Value>) -> Result<(), ErrorReport> {
        let mut errors = Vec::new();
        for example_playload in &self.examples {
            if let Err(example_error) = example_playload.validate(definitions) {
                errors.push(example_error);
            }
        }
        match NonEmpty::from_vec(errors) {
            None => Ok(()),
            Some(errors) => {
                let mut report = IndexMap::new();
                report.insert(self.operation_id.clone(), errors);
                Err(report)
            }
        }
    }
    fn from_operation(
        operation_id: OperationId,
        operation: &openapiv3::Operation,
        components: &Components,
    ) -> Option<OperationExamples> {
        let mut examples: Vec<ExamplePayload> = Vec::new();

        for (status_code, ref_or_response) in &operation.responses.responses {
            if is_success(status_code) {
                if let Some(response) = extract_or_resolve(&components.responses, ref_or_response) {
                    for example in ExamplePayload::from_media_types(
                        &response.content,
                        &components.schemas,
                        false,
                    ) {
                        examples.push(example);
                    }
                }
            }
        }

        if let Some(request_body) = operation
            .request_body
            .as_ref()
            .and_then(|request_body| extract_or_resolve(&components.requests, &request_body))
        {
            for example in
                ExamplePayload::from_media_types(&request_body.content, &components.schemas, true)
            {
                examples.push(example);
            }
        }

        NonEmpty::from_vec(examples).map(|examples| OperationExamples {
            operation_id,
            examples,
        })
    }
}
pub(crate) fn validate_from_spec(spec: &OpenAPI) -> Result<(), ErrorReport> {
    let schemas = clone_items(
        spec.components
            .as_ref()
            .map(|components| &components.schemas),
    );
    let responses = clone_items(
        spec.components
            .as_ref()
            .map(|components| &components.responses),
    );
    let requests = clone_items(
        spec.components
            .as_ref()
            .map(|components| &components.request_bodies),
    );
    let components = Components {
        schemas,
        responses,
        requests,
    };

    let path_items = clone_items(Some(&spec.paths));
    let mut report = IndexMap::new();
    let mut schemas = IndexMap::new();
    for (key, schema) in &components.schemas {
        schemas.insert(
            key.to_owned(),
            serde_json::to_value(&schema).expect("Cannot serialise schema to json"),
        );
    }

    for (path, path_item) in path_items {
        for (operation_id, operation) in operations_for(&path, &path_item) {
            if let Some(examples) =
                OperationExamples::from_operation(operation_id, operation, &components)
            {
                if let Err(operation_report) = examples.validate(&schemas) {
                    report.extend(operation_report);
                }
            }
        }
    }
    if report.len() == 0 {
        Ok(())
    } else {
        Err(report)
    }
}

fn is_json_media_type(media_type: &str) -> bool {
    if let Ok(media_type) = media_type.parse::<mime::Mime>() {
        media_type == mime::APPLICATION_JSON
            || media_type.type_() == mime::APPLICATION && media_type.suffix() == Some(mime::JSON)
    } else {
        false
    }
}

fn is_success(code: &StatusCode) -> bool {
    format!("{}", code).starts_with("2")
}

fn clone_items<T: Clone>(
    components: Option<&IndexMap<String, ReferenceOr<T>>>,
) -> IndexMap<String, T> {
    let mut values_by_key: IndexMap<String, T> = IndexMap::new();
    if let Some(components) = components {
        for (key, reference_or_t) in components {
            match reference_or_t {
                ReferenceOr::Item(t) => {
                    values_by_key.insert(key.to_owned(), t.clone());
                }
                _ => (),
            }
        }
    }
    values_by_key
}

fn operations_for<'s>(
    path: &str,
    path_item: &'s openapiv3::PathItem,
) -> Vec<(OperationId, &'s openapiv3::Operation)> {
    HttpMethod::all()
        .iter()
        .filter_map(|method| {
            operation_for(method, path_item).map(|operation| {
                let operation_id = OperationId::try_from(operation)
                    .unwrap_or_else(|_| OperationId::for_method_and_path(method, path));
                (operation_id, operation)
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    //TODO: for integration tests
    // let data = include_str!("../petstore.json");

    #[test]
    fn test_operation_id() {
        assert_eq!(
            OperationId::for_method_and_path(&HttpMethod::PUT, &"/some/resource"),
            OperationId("putSomeResource".to_owned())
        );

        assert_eq!(
            OperationId::for_method_and_path(&HttpMethod::GET, &"thing"),
            OperationId("getThing".to_owned())
        );

        assert_eq!(
            OperationId::for_method_and_path(&HttpMethod::POST, &""),
            OperationId("post".to_owned())
        );
    }

    #[test]
    fn test_is_json_media_type() {
        assert!(is_json_media_type("application/json"));
        assert!(is_json_media_type("application/vnd.api+json"));
        assert!(is_json_media_type("text/plain") == false);
    }

    #[test]
    fn extract_or_resolve_gets_single_components() {
        let mut test_component: IndexMap<String, char> = IndexMap::new();

        test_component.insert("key-a".to_owned(), 'a');
        test_component.insert("key-b".to_owned(), 'b');

        assert_eq!(
            extract_or_resolve(&test_component, &ReferenceOr::Item('x')),
            Some(&'x')
        );
        let reference = "#/components/test/key-a".to_owned();
        assert_eq!(
            extract_or_resolve(&test_component, &ReferenceOr::Reference { reference }),
            Some(&'a')
        );
    }
    #[test]
    fn recursively_expands_schema_refs() {
        let mut definitions: IndexMap<String, Value> = IndexMap::new();
        definitions.insert(
            "Entity1".to_owned(),
            json!({"$ref": "#/components/schemas/Entity2"}),
        );
        definitions.insert("Entity2".to_owned(), json!({"type": "string"}));

        let mut schema = json!(
            {
                "properties": {
                  "connections": {
                    "items": {
                      "$ref": "#/components/schemas/Entity1"
                    },
                    "type": "array"
                  }
                },
                "type": "object"
              }
        );
        expand_schema_refs(&mut schema, &definitions);
        assert_eq!(
            schema.pointer("/properties/connections/items"),
            Some(&json!({"type": "string"}))
        )
    }
}
