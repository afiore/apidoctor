use jsonschema::JSONSchema;
use nonempty::NonEmpty;
use openapiv3::Components;
use openapiv3::MediaType;
use openapiv3::Paths;
use openapiv3::ReferenceOr;
use openapiv3::RequestBody;
use openapiv3::Response;
use openapiv3::Schema;
use openapiv3::StatusCode;
use serde_json::Value;
use std::collections::HashMap;
use std::convert::identity;

pub(crate) enum Ior<A, B> {
    Left(A),
    Right(B),
    Both(A, B),
}

struct ExampleWithSchema {
    example: Value,
    schema: Value,
}

struct WithSchemas<'s, T> {
    schemas: &'s HashMap<String, Schema>,
    value: T,
}
fn extract_or_resolve<'a, T>(
    all_by_ref: &'a HashMap<String, T>,
    ref_or_value: &'a ReferenceOr<T>,
) -> Option<&'a T> {
    extract_or_apply(all_by_ref, ref_or_value, |t| t)
}

fn extract_or_apply<'a, T, S, F>(
    all_by_ref: &'a HashMap<String, S>,
    ref_or_value: &'a ReferenceOr<T>,
    f: F,
) -> Option<&'a S>
where
    F: Fn(&'a T) -> &'a S,
{
    match ref_or_value {
        ReferenceOr::Item(t) => Some(f(t)),
        ReferenceOr::Reference { reference } => all_by_ref.get(reference),
    }
}

//TODO: introduce lifetimes
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

type Examples = Ior<ExampleWithSchema, ExampleWithSchema>;

pub(crate) struct Operation {
    path: String,
    method: String,
}

pub(crate) struct OperationWithErrors {
    operation: Operation,
    example: Value,
    errors: Vec<ValidationError>,
}

pub struct ValidationError {
    path: String,
    message: String,
}
impl<'a> From<&'a jsonschema::ValidationError<'a>> for ValidationError {
    fn from(schema_err: &'a jsonschema::ValidationError<'a>) -> Self {
        ValidationError {
            path: format!("{}", &schema_err.schema_path),
            message: format!("{}", &schema_err),
        }
    }
}

struct ExamplePayload {
    is_request: bool,
    example: ExampleWithSchema,
}

struct OperationExamples {
    operation: Operation,
    examples: NonEmpty<ExamplePayload>,
}
impl OperationExamples {
    fn from_openapi(
        path: &str,
        method: &str,
        op: &openapiv3::Operation,
        schemas: &HashMap<String, Schema>,
        responses: &HashMap<String, Response>,
        requests: &HashMap<String, RequestBody>,
    ) -> Option<OperationExamples> {
        //TODO: come up with a better way to identify the response payload
        let success_response = op
            .responses
            .responses
            .get(&StatusCode::Code(200))
            .and_then(|ref_or_response| extract_or_resolve(responses, ref_or_response));

        let response_example: Option<ExamplePayload> = success_response.and_then(|response| {
            response
                .content
                //TODO: find a more solid way to extract
                .get("application/json")
                .and_then(|media_type| {
                    Option::from(WithSchemas {
                        schemas,
                        value: media_type.clone(), //avoid clone?
                    })
                })
                .map(|example| ExamplePayload {
                    is_request: false,
                    example,
                })
        });

        let request_example: Option<ExamplePayload> = op
            .request_body
            .as_ref()
            .and_then(|request_body| extract_or_resolve(requests, &request_body))
            //TODO: find a way to support custom content types (e.g. application/vnd.foobar+json;)
            .and_then(|request_body| request_body.content.get("application/json"))
            .and_then(|media_type| {
                Option::from(WithSchemas {
                    schemas,
                    value: media_type.clone(), //avoid clone?
                })
            })
            .map(|example| ExamplePayload {
                is_request: true,
                example,
            });

        let mut examples: Vec<ExamplePayload> = request_example.into_iter().collect();
        let mut response_examples: Vec<ExamplePayload> = response_example.into_iter().collect();
        examples.append(&mut response_examples);

        let operation = Operation {
            path: path.to_owned(),
            method: method.to_owned(),
        };

        NonEmpty::from_vec(examples).map(|examples| OperationExamples {
            operation,
            examples,
        })
    }
}

fn validate(
    _schema: &JSONSchema,
    _operation: Operation,
    _examples: Examples,
) -> Result<(), OperationWithErrors> {
    // schema.validate(&example).map_err(|errors| {
    //     let validation_errors = errors.map(|err| ValidationError::from(&err)).collect();
    //     OperationWithErrors {
    //         operation,
    //         validation_errors,
    //     }
    // })
    Ok(())
}
