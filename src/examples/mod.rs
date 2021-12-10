use std::error::Error;
use std::fmt::Display;
use std::io;

use crate::openapi::is_success;
use crate::openapi::Components;
use crate::openapi::OperationId;
use indexmap::IndexMap;
use jsonschema::JSONSchema;
use nonempty::NonEmpty;
use openapiv3::MediaType;
use openapiv3::Operation;
use openapiv3::ReferenceOr;
use serde_json::Value;

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

#[derive(thiserror::Error, Debug)]
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

impl Error for ExampleError {}

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

#[derive(Debug, PartialEq)]
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

//TODO: refactor as follows:
// - return a new value instead of mutating input
// - incrementally expand definitions, so that references are resolved only once
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

    fn from_media_type(
        media_type: &MediaType,
        schemas: &IndexMap<String, Value>,
        is_request: bool,
    ) -> Option<Self> {
        let example = media_type.example.as_ref()?;
        let schema_or_ref = media_type.schema.as_ref().map(|x| match x {
            ReferenceOr::Item(schema) => ReferenceOr::Item(
                serde_json::to_value(schema)
                    .expect(&format!("Cannot serialise as JSON: {:?}", schema)),
            ),
            ReferenceOr::Reference { reference } => ReferenceOr::Reference {
                reference: reference.to_owned(),
            },
        })?;

        extract_or_resolve(&schemas, &schema_or_ref).map(|schema| {
            let schema = serde_json::to_value(schema)
                .expect(&format!("Cannot serialise as JSON: {:?}", schema));
            ExamplePayload {
                //TODO: review clone
                example: example.clone(),
                schema,
                is_request,
            }
        })
    }

    fn from_media_types(
        media_types: &IndexMap<String, MediaType>,
        schemas: &IndexMap<String, Value>,
        is_request: bool,
    ) -> Vec<ExamplePayload> {
        let mut examples = Vec::new();
        for (media_type, media_type_response) in media_types {
            if is_json_media_type(media_type) {
                if let Some(example) =
                    ExamplePayload::from_media_type(media_type_response, schemas, is_request)
                {
                    examples.push(example);
                }
            }
        }
        examples
    }
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

#[derive(Debug)]
pub(crate) struct ExamplePayloads {
    operation_id: OperationId,
    examples: NonEmpty<ExamplePayload>,
}
impl ExamplePayloads {
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

    //need unit test
    pub(crate) async fn from_operations(
        operations: &Vec<(OperationId, &Operation)>,
        components: &Components,
    ) -> Result<(), ErrorReport> {
        let mut report = IndexMap::new();

        for (operation_id, operation) in operations {
            if let Some(examples) = Self::from_operation(operation_id, operation, &components) {
                if let Err(operation_report) = examples.validate(&components.schemas) {
                    report.extend(operation_report);
                }
            }
        }
        if report.len() == 0 {
            Ok(())
        } else {
            Err(report)
        }
    }

    fn from_operation(
        operation_id: &OperationId,
        operation: &openapiv3::Operation,
        components: &Components,
    ) -> Option<ExamplePayloads> {
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

        NonEmpty::from_vec(examples).map(|examples| ExamplePayloads {
            operation_id: operation_id.clone(),
            examples,
        })
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

fn payload_schemas_without_example(content: &IndexMap<String, MediaType>) -> Option<Value> {
    content
        .iter()
        .find_map(|(media_type, media_type_response)| {
            if is_json_media_type(media_type) && media_type_response.example.is_none() {
                media_type_response
                    .schema
                    .as_ref()
                    .and_then(|schema| serde_json::to_value(schema).ok())
            } else {
                None
            }
        })
}

#[derive(Debug)]
pub(crate) struct SchemaNeedsExample {
    schema: Value,
    is_request: bool,
}

impl SchemaNeedsExample {
    fn request(schema: Value) -> Self {
        SchemaNeedsExample {
            schema,
            is_request: true,
        }
    }
    fn response(schema: Value) -> Self {
        SchemaNeedsExample {
            schema,
            is_request: false,
        }
    }
}

impl Display for SchemaNeedsExample {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let request_response = if self.is_request {
            "request"
        } else {
            "response"
        };
        write!(
            f,
            "{} schema needs an example:\n\n{}\n",
            request_response,
            serde_json::to_string_pretty(&self.schema).expect("Cannot serialise JSON")
        )
    }
}

impl std::error::Error for SchemaNeedsExample {}

fn needs_example(operation: &Operation) -> Option<SchemaNeedsExample> {
    let response_schema_without_example = operation.responses.responses.iter().find_map(
        |(status_code, ref_or_item)| match ref_or_item {
            ReferenceOr::Item(response) if is_success(status_code) => {
                payload_schemas_without_example(&response.content).map(SchemaNeedsExample::response)
            }
            _ => None,
        },
    );

    let request_schema_without_example = match &operation.request_body {
        Some(ReferenceOr::Item(request)) => {
            payload_schemas_without_example(&request.content).map(SchemaNeedsExample::request)
        }
        _ => None,
    };

    response_schema_without_example.or_else(|| request_schema_without_example)
}

//TODO: add unit test
pub(crate) async fn need_example(
    operations: &Vec<(OperationId, &Operation)>,
) -> Result<(), IndexMap<OperationId, SchemaNeedsExample>> {
    let mut operation_ids = IndexMap::new();

    for (operation_id, operation) in operations {
        if let Some(schema_needs_example) = needs_example(operation) {
            operation_ids.insert(operation_id.clone(), schema_needs_example);
        }
    }
    if operation_ids.len() == 0 {
        Ok(())
    } else {
        Err(operation_ids)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    //TODO: for integration tests
    // let data = include_str!("../petstore.json");

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

    #[test]
    fn example_payload_from_media_types() {
        let schema = ReferenceOr::Reference {
            reference: "#/components/schema/testSchema".to_owned(),
        };

        let mut schemas = IndexMap::new();
        schemas.insert("testSchema".to_owned(), json!({"type": "object"}));

        let media_type_no_example = MediaType {
            schema: Some(schema.clone()),
            example: None,
            examples: IndexMap::new(),
            encoding: IndexMap::new(),
        };

        let mut media_types = IndexMap::new();
        media_types.insert("application/json".to_owned(), media_type_no_example);

        assert_eq!(
            ExamplePayload::from_media_types(&media_types, &schemas, true).len(),
            0
        );

        let example_payload = json!("someJsonPayload");

        let media_type_with_example = MediaType {
            schema: Some(schema.clone()),
            example: Some(example_payload.clone()),
            examples: IndexMap::new(),
            encoding: IndexMap::new(),
        };
        media_types.insert(
            "application/vnd.my.example.api+json".to_owned(),
            media_type_with_example,
        );
        assert_eq!(
            ExamplePayload::from_media_types(&media_types, &schemas, true),
            vec![ExamplePayload {
                is_request: true,
                example: example_payload.clone(),
                schema: json!({"type": "object"})
            }]
        );
    }
}
