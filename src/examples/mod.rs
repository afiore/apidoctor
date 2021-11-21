use indexmap::IndexMap;
use jsonschema::JSONSchema;
use nonempty::NonEmpty;
use openapiv3::MediaType;
use openapiv3::OpenAPI;
use openapiv3::PathItem;
use openapiv3::ReferenceOr;
use openapiv3::RequestBody;
use openapiv3::Response;
use openapiv3::Schema;
use openapiv3::StatusCode;
use serde_json::Value;

#[derive(Debug)]
pub struct ValidationError {
    field_path: String,
    message: String,
}
impl<'a> From<&'a jsonschema::ValidationError<'a>> for ValidationError {
    fn from(schema_err: &'a jsonschema::ValidationError<'a>) -> Self {
        ValidationError {
            field_path: format!("{}", &schema_err.schema_path),
            message: format!("{}", &schema_err),
        }
    }
}

#[derive(Debug)]
struct ExampleWithSchema {
    example: Value,
    schema: Value,
}

impl ExampleWithSchema {
    fn validate(&self) -> Result<(), NonEmpty<ValidationError>> {
        let compiled_schema = JSONSchema::compile(&self.schema).expect("Cannot parse schema");
        let result = compiled_schema.validate(&self.example);

        if let Err(errors) = result {
            let errors = NonEmpty::from_vec(
                errors
                    .map(|err| ValidationError::from(&err))
                    .collect::<Vec<ValidationError>>(),
            )
            .expect("non-empty error list expected");
            Err(errors)
        } else {
            Ok(())
        }
    }
    fn into_example_payload(self, is_request: bool) -> ExamplePayload {
        ExamplePayload {
            is_request,
            example: self,
        }
    }
}

#[derive(Debug)]
struct ExamplePayload {
    is_request: bool,
    example: ExampleWithSchema,
}

impl ExamplePayload {
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
            //TODO: strip the entire prefix for all component types
            let reference = reference.replace("#/components/schemas/", "");
            println!("getting ref {}", reference);

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

#[derive(PartialEq, Clone, Debug)]
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

#[derive(Debug)]
pub(crate) struct OperationId {
    path: String,
    method: HttpMethod,
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
    fn validate(&self) -> Result<(), NonEmpty<ValidationError>> {
        for example_playload in &self.examples {
            let result = example_playload.example.validate();
            if result.is_err() {
                return result;
            } else {
                continue;
            }
        }
        Ok(())
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

        println!("{:?}", &operation.request_body);

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
pub fn validate_from_spec(spec: &OpenAPI) -> Result<(), NonEmpty<ValidationError>> {
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

    for (path, path_item) in path_items {
        for (operation_id, operation) in operations_for(&path, &path_item) {
            if let Some(examples) =
                OperationExamples::from_operation(operation_id, operation, &components)
            {
                println!("got ops: {:?}", examples);
                let result = examples.validate();
                if result.is_err() {
                    return result;
                }
            }
        }
    }
    Ok(())
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
        values_by_key
    } else {
        values_by_key
    }
}

fn operations_for<'s>(
    path: &str,
    path_item: &'s openapiv3::PathItem,
) -> Vec<(OperationId, &'s openapiv3::Operation)> {
    HttpMethod::all()
        .iter()
        .filter_map(|method| {
            operation_for(method, path_item).map(|operation| {
                (
                    OperationId {
                        method: method.to_owned(),
                        path: path.to_owned(),
                    },
                    operation,
                )
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_json_media_type() {
        assert!(is_json_media_type("application/json"));
        assert!(is_json_media_type("application/vnd.api+json"));
        assert!(is_json_media_type("text/plain") == false);
    }
}
