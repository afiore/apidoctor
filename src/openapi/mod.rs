use std::{
    convert::Infallible, error::Error, fmt::Display, fs::File, io::BufReader, path::Path,
    str::FromStr,
};

use indexmap::IndexMap;
use nonempty::NonEmpty;
use openapiv3::{OpenAPI, Operation, PathItem, ReferenceOr, RequestBody, Response, StatusCode};
use serde_json::Value;

use crate::{
    examples::{need_example, ExamplePayloads},
    Tag,
};

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
pub(crate) struct OperationId(pub(crate) String);

impl Display for OperationId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for OperationId {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(OperationId(s.to_owned()))
    }
}

impl TryFrom<&Operation> for OperationId {
    type Error = ();

    fn try_from(op: &Operation) -> Result<Self, Self::Error> {
        op.operation_id
            .as_ref()
            .map(|id| OperationId(id.to_owned()))
            .ok_or(())
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

pub(crate) fn spec_from_file<P: AsRef<Path>>(path: P) -> Result<OpenAPI, Box<dyn Error>> {
    let file = File::open(path.as_ref())?;
    let reader = BufReader::new(file);
    let extension = path.as_ref().extension().and_then(|ext| ext.to_str());

    if let Some("yml") | Some("yaml") = extension {
        serde_yaml::from_reader(reader).map_err(Into::into)
    } else {
        serde_json::from_reader(reader).map_err(Into::into)
    }
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
    operations_with_invalid_examples: u16,
}

impl Default for Stats {
    fn default() -> Self {
        Self {
            total_operations: Default::default(),
            operations_needing_examples: Default::default(),
            operations_with_invalid_examples: Default::default(),
        }
    }
}

impl Display for Stats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "API Stats\n")?;
        writeln!(
            f,
            "{:<15}: {:>3}",
            "Total operations", self.total_operations
        )?;
        writeln!(
            f,
            "{:<15}: {:>3}",
            "Needing examples", self.operations_needing_examples
        )?;
        writeln!(
            f,
            "{:<15}: {:>3}",
            "Invalid examples", self.operations_with_invalid_examples
        )?;

        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct LintingIssues {
    pub tags: Vec<Tag>,
    pub issues: NonEmpty<Box<dyn Error>>,
}

type OperationLintingIssues = IndexMap<OperationId, LintingIssues>;

#[derive(Debug)]
pub(crate) enum LintingOutcome {
    AllGood(Stats),
    IssuesFound {
        stats: Stats,
        operation_linting_issues: OperationLintingIssues,
    },
    OperationNotFound(OperationId),
}

pub(crate) async fn lint(
    spec: &OpenAPI,
    tags: Vec<Tag>,
    operation_id: Option<OperationId>,
) -> LintingOutcome {
    let components = spec
        .components
        .as_ref()
        .map(Components::new)
        .unwrap_or_default();

    let mut stats = Stats::default();
    let mut operation_linting_issues = IndexMap::new();

    let path_items = clone_items(&spec.paths);

    let cli_filter: Box<dyn Fn(&Operation) -> bool> = match (&operation_id, &tags) {
        (Some(operation_id), _) => Box::new(|operation: &Operation| {
            operation.operation_id == Some(format!("{}", operation_id.clone()))
        }),
        (_, tags) => Box::new(|operation: &Operation| {
            if tags.len() == 0 {
                true
            } else {
                tags.iter().any(|tag| operation.tags.contains(&tag.0))
            }
        }),
    };

    let operations: Vec<(OperationId, &Operation)> = path_items
        .iter()
        .flat_map(|(path, path_item)| operations_for(path, path_item))
        .filter(|(_, operation)| cli_filter(operation))
        .collect();

    let mut operation_tags: IndexMap<OperationId, Vec<Tag>> = IndexMap::new();
    for (operation_id, operation) in operations.iter() {
        let tags = operation.tags.iter().map(|s| Tag(s.clone())).collect();
        operation_tags.insert(operation_id.to_owned(), tags);
    }

    stats.total_operations += operations.len() as u16;

    let (operation_examples_result, needing_example_result) = futures::join!(
        ExamplePayloads::from_operations(&operations, &components),
        need_example(&operations)
    );

    if let Err(operation_report) = operation_examples_result {
        let operation_report: OperationLintingIssues = operation_report
            .into_iter()
            .map(|(op_id, errors)| {
                let issues: NonEmpty<Box<dyn Error>> = errors.map(|e| e.into());

                let tags = operation_tags
                    .get(&op_id)
                    .map(Clone::clone)
                    .unwrap_or_default();

                let issues = LintingIssues { tags, issues };
                (op_id.to_owned(), issues)
            })
            .collect();

        stats.operations_with_invalid_examples += operation_report.len() as u16;

        operation_linting_issues.extend(operation_report);
    }

    if let Err(needing_examples) = needing_example_result {
        stats.operations_needing_examples += needing_examples.len() as u16;

        for (operation_id, schema_needing_example) in needing_examples {
            if let Some(LintingIssues { tags: _, issues }) =
                operation_linting_issues.get_mut(&operation_id)
            {
                issues.push(schema_needing_example.into());
            } else {
                let tags = operation_tags
                    .get(&operation_id)
                    .map(Clone::clone)
                    .unwrap_or_default();

                let linting_issues = LintingIssues {
                    tags,
                    issues: NonEmpty::new(schema_needing_example.into()),
                };
                operation_linting_issues.insert(operation_id, linting_issues);
            }
        }
    }

    match (&operation_id, operation_linting_issues.len()) {
        (Some(operation_id), 0) => LintingOutcome::OperationNotFound(operation_id.to_owned()),
        (_, 0) => LintingOutcome::AllGood(stats),
        _ => LintingOutcome::IssuesFound {
            stats,
            operation_linting_issues,
        },
    }
}
