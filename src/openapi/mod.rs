use std::{error::Error, fmt::Display};

use indexmap::IndexMap;
use nonempty::NonEmpty;
use openapiv3::{OpenAPI, Operation, PathItem, ReferenceOr, RequestBody, Response, StatusCode};
use serde_json::Value;

use crate::examples::{need_example, ExamplePayloads};

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

pub(crate) fn operations_for<'s>(
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

pub(crate) fn is_success(code: &StatusCode) -> bool {
    format!("{}", code).starts_with("2")
}

#[cfg(test)]
mod tests {
    use super::*;
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
}

#[derive(Debug)]
pub(crate) struct Components {
    pub schemas: IndexMap<String, Value>,
    pub responses: IndexMap<String, Response>,
    pub requests: IndexMap<String, RequestBody>,
}

impl Default for Components {
    fn default() -> Self {
        Self {
            schemas: Default::default(),
            responses: Default::default(),
            requests: Default::default(),
        }
    }
}

impl Components {
    pub(crate) fn new(inner: &openapiv3::Components) -> Self {
        let schemas: IndexMap<String, Value> = clone_items(&inner.schemas)
            .iter()
            .map(|(key, schema)| {
                (
                    key.to_owned(),
                    serde_json::to_value(&schema).expect("Cannot serialise schema to json"),
                )
            })
            .collect();

        let responses = clone_items(&inner.responses);
        let requests = clone_items(&inner.request_bodies);

        Components {
            schemas,
            responses,
            requests,
        }
    }
}
pub(crate) fn clone_items<T: Clone>(
    components: &IndexMap<String, ReferenceOr<T>>,
) -> IndexMap<String, T> {
    let mut values_by_key: IndexMap<String, T> = IndexMap::new();
    for (key, reference_or_t) in components {
        match reference_or_t {
            ReferenceOr::Item(t) => {
                values_by_key.insert(key.to_owned(), t.clone());
            }
            _ => (),
        }
    }
    values_by_key
}

#[derive(Debug)]
pub(crate) struct Stats {
    total_operations: u16,
    operations_needing_examples: u16,
}

impl Default for Stats {
    fn default() -> Self {
        Self {
            total_operations: Default::default(),
            operations_needing_examples: Default::default(),
        }
    }
}

type OperationErrors = IndexMap<OperationId, NonEmpty<Box<dyn Error>>>;

#[derive(Debug)]
pub(crate) struct ValidationOutcome {
    pub stats: Stats,
    pub result: Result<(), OperationErrors>,
}

impl ValidationOutcome {
    fn ok(stats: Stats) -> Self {
        ValidationOutcome {
            stats,
            result: Ok(()),
        }
    }
    fn error(stats: Stats, report: OperationErrors) -> Self {
        ValidationOutcome {
            stats,
            result: Err(report),
        }
    }
}

pub(crate) async fn validate_from_spec(spec: &OpenAPI) -> ValidationOutcome {
    let components = spec
        .components
        .as_ref()
        .map(Components::new)
        .unwrap_or_default();

    let mut stats = Stats::default();
    let mut report = IndexMap::new();

    let path_items = clone_items(&spec.paths);

    let operations: Vec<(OperationId, &Operation)> = path_items
        .iter()
        .flat_map(|(path, path_item)| operations_for(path, path_item))
        .collect();

    stats.total_operations += operations.len() as u16;

    let (operation_examples_result, needing_example_result) = futures::join!(
        ExamplePayloads::from_operations(&operations, &components),
        need_example(&operations)
    );

    if let Err(operation_report) = operation_examples_result {
        let operation_report: OperationErrors = operation_report
            .into_iter()
            .map(|(op_id, errors)| (op_id.to_owned(), errors.map(|e| e.into())))
            .collect();

        report.extend(operation_report);
    }

    if let Err(needing_examples) = needing_example_result {
        stats.operations_needing_examples += needing_examples.len() as u16;

        for (operation_id, schema_needing_example) in needing_examples {
            if let Some(errors) = report.get_mut(&operation_id) {
                errors.push(schema_needing_example.into());
            } else {
                report.insert(operation_id, NonEmpty::new(schema_needing_example.into()));
            }
        }
    }

    if report.len() == 0 {
        ValidationOutcome::ok(stats)
    } else {
        ValidationOutcome::error(stats, report)
    }
}
