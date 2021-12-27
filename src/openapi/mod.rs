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
pub(crate) enum HttpMethod {
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

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub(crate) struct OperationContext {
    pub id: OperationId,
    pub method: HttpMethod,
    pub path: String,
    pub tags: Vec<Tag>,
}

#[derive(Debug)]
pub(crate) struct OperationWithContext {
    pub context: OperationContext,
    pub operation: Operation,
}

struct OperationIterator {
    path_items: Vec<(String, PathItem)>,
    current_item_operations: Option<NonEmpty<OperationWithContext>>,
    current_item: usize,
}

impl OperationIterator {
    fn new(path_items: Vec<(String, PathItem)>) -> Self {
        OperationIterator {
            path_items,
            current_item_operations: None,
            current_item: 0,
        }
    }
}

impl Iterator for OperationIterator {
    type Item = OperationWithContext;

    fn next(&mut self) -> Option<Self::Item> {
        //we have iterated over all the path items, nothing else to do
        if self.current_item > self.path_items.len() {
            None
        } else {
            //if there's some operations for the current PathItem,
            if let Some(current_item_ops) = self.current_item_operations.take() {
                //set current item operations to the operations' tail
                self.current_item_operations = NonEmpty::from_vec(current_item_ops.tail);

                Some(current_item_ops.head)
            } else {
                let mut next_op = None;

                loop {
                    if &self.current_item >= &self.path_items.len() {
                        break;
                    }

                    let (path, path_item) = &self.path_items[self.current_item];
                    self.current_item += 1;

                    let operations: Option<NonEmpty<OperationWithContext>> = NonEmpty::from_vec(
                        operations_for(&path, &path_item)
                            .into_iter()
                            .map(|(context, operation)| OperationWithContext {
                                context,
                                operation: operation.to_owned(),
                            })
                            .collect(),
                    );

                    if let Some(operations) = operations {
                        self.current_item_operations = NonEmpty::from_vec(operations.tail);
                        next_op = Some(operations.head);
                        break;
                    }
                }

                next_op
            }
        }
    }
}

fn operations_for<'s>(
    path: &str,
    path_item: &'s openapiv3::PathItem,
) -> Vec<(OperationContext, &'s openapiv3::Operation)> {
    HttpMethod::all()
        .iter()
        .filter_map(|method| {
            operation_for(method, path_item).map(|operation| {
                let id = OperationId::try_from(operation)
                    .unwrap_or_else(|_| OperationId::for_method_and_path(method, path));
                let tags = operation.tags.iter().map(|s| Tag(s.to_owned())).collect();
                let context = OperationContext {
                    id,
                    tags,
                    method: method.to_owned(),
                    path: path.to_owned(),
                };
                (context, operation)
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

#[derive(Debug, Default)]
pub(crate) struct Components {
    pub schemas: IndexMap<String, Value>,
    pub responses: IndexMap<String, Response>,
    pub requests: IndexMap<String, RequestBody>,
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

#[derive(Debug, Default)]
pub(crate) struct Stats {
    total_operations: u16,
    matching_filters: u16,
    operations_needing_examples: u16,
    operations_with_invalid_examples: u16,
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
            "Matching filters", self.matching_filters
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
    //TODO: replace box in favour of an enum
    pub issues: NonEmpty<Box<dyn Error>>,
}

type OperationLintingIssues = IndexMap<OperationContext, LintingIssues>;

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

    let path_items: Vec<(String, PathItem)> = clone_items(&spec.paths).into_iter().collect();

    let cli_filter: Box<dyn Fn(&Operation) -> bool> = match (&operation_id, &tags) {
        (Some(operation_id), _) => Box::new(|operation: &Operation| {
            operation.operation_id == Some(operation_id.to_string())
        }),
        (_, tags) => Box::new(|operation: &Operation| {
            if tags.len() == 0 {
                true
            } else {
                tags.iter().any(|tag| operation.tags.contains(&tag.0))
            }
        }),
    };

    let operations: Vec<OperationWithContext> = OperationIterator::new(path_items).collect();
    stats.total_operations = operations.len() as u16;

    let operations: Vec<OperationWithContext> = operations
        .into_iter()
        .filter(|op| cli_filter(&op.operation))
        .collect();

    stats.matching_filters = operations.len() as u16;

    //shortcut with OperationNotFound if supplied filter yields no result
    if let (Some(operation_id), 0) = (operation_id.clone(), operations.len()) {
        return LintingOutcome::OperationNotFound(operation_id);
    }

    let (operation_examples_result, needing_example_result) = futures::join!(
        ExamplePayloads::from_operations(&operations, &components),
        need_example(&operations)
    );

    if let Err(operation_report) = operation_examples_result {
        let operation_report: OperationLintingIssues = operation_report
            .into_iter()
            .map(|(op_id, errors)| {
                let issues: NonEmpty<Box<dyn Error>> = errors.map(|e| e.into());

                let issues = LintingIssues { issues };
                (op_id.to_owned(), issues)
            })
            .collect();

        stats.operations_with_invalid_examples += operation_report.len() as u16;

        operation_linting_issues.extend(operation_report);
    }

    if let Err(needing_examples) = needing_example_result {
        stats.operations_needing_examples += needing_examples.len() as u16;

        for (operation_id, schema_needing_example) in needing_examples {
            if let Some(LintingIssues { issues }) = operation_linting_issues.get_mut(&operation_id)
            {
                issues.push(schema_needing_example.into());
            } else {
                let linting_issues = LintingIssues {
                    issues: NonEmpty::new(schema_needing_example.into()),
                };
                operation_linting_issues.insert(operation_id, linting_issues);
            }
        }
    }

    if operation_linting_issues.len() == 0 {
        LintingOutcome::AllGood(stats)
    } else {
        LintingOutcome::IssuesFound {
            stats,
            operation_linting_issues,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;
    use tempdir::TempDir;

    #[test]
    fn operation_id_for_method_and_path() {
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

    fn test_operation(id: String) -> openapiv3::Operation {
        let mut operation = openapiv3::Operation::default();
        operation.operation_id = Some(id);
        operation
    }

    fn built_path_item(update: impl FnOnce(&mut PathItem)) -> PathItem {
        let mut path_item = PathItem::default();
        update(&mut path_item);
        path_item
    }

    fn spec_file_roundtrip<F: Fn(&OpenAPI) -> String>(
        dir: &TempDir,
        spec: &OpenAPI,
        filename: &str,
        serialize: F,
    ) {
        let file_path = dir.path().join(filename);
        let file_path = file_path.as_path();
        let mut spec_file = File::create(file_path).unwrap();
        spec_file.write_all(serialize(spec).as_bytes()).unwrap();

        if let Ok(parsed_spec) = spec_from_file(file_path) {
            assert_eq!(&parsed_spec, spec);
        } else {
            panic!("spec file expected");
        }
    }

    #[test]
    fn test_spec_from_yaml() {
        let spec = OpenAPI::default();
        let dir = TempDir::new("apidoctor").unwrap();
        let serialize = |spec: &OpenAPI| serde_yaml::to_string(spec).unwrap();

        let file_names = vec!["spec.yaml", "spec.yml"];

        for file_name in file_names {
            spec_file_roundtrip(&dir, &spec, file_name, serialize);
        }
    }

    #[test]
    fn test_spec_from_json() {
        let spec = OpenAPI::default();
        let dir = TempDir::new("apidoctor").unwrap();
        let serialize = |spec: &OpenAPI| serde_json::to_string(spec).unwrap();

        spec_file_roundtrip(&dir, &spec, "spec.json", serialize);
    }

    #[test]
    fn operations_iterator() {
        let path_items = vec![
            (
                "a".to_owned(),
                built_path_item(|pi| {
                    pi.get = Some(test_operation("getA".to_owned()));
                    pi.delete = Some(test_operation("deleteA".to_owned()));
                    pi.post = Some(test_operation("postA".to_owned()));
                }),
            ),
            ("a1".to_owned(), built_path_item(|_| {})),
            (
                "b".to_owned(),
                built_path_item(|pi| {
                    pi.get = Some(test_operation("getB".to_owned()));
                }),
            ),
            ("c".to_owned(), built_path_item(|_| {})),
        ];

        let operation_ids: Vec<String> = OperationIterator::new(path_items)
            .map(|op| op.context.id.to_string())
            .collect();

        assert_eq!(operation_ids, vec!["getA", "postA", "deleteA", "getB"])
    }

    #[test]
    fn lint_fails_on_unknown_operation_id() {
        let operation_id = OperationId("unknown".to_owned());

        if let LintingOutcome::OperationNotFound(op_id) = futures::executor::block_on(lint(
            &OpenAPI::default(),
            vec![],
            Some(operation_id.clone()),
        )) {
            assert_eq!(&op_id, &operation_id)
        } else {
            panic!("LintingOutcome::OperationNotFound expected!")
        }
    }

    #[test]
    fn lint_finds_a_given_operation_id() {
        let op_id = "example";
        let operation_id = OperationId(op_id.to_owned());

        let mut spec = OpenAPI::default();
        let path1 = built_path_item(|pi| {
            pi.get = Some(test_operation("other".to_owned()));
        });
        let path2 = built_path_item(|pi| {
            pi.post = Some(test_operation(op_id.to_owned()));
        });

        spec.paths
            .insert("/path/1".to_owned(), ReferenceOr::Item(path1));

        spec.paths
            .insert("/path/2".to_owned(), ReferenceOr::Item(path2));

        if let LintingOutcome::AllGood(stats) =
            futures::executor::block_on(lint(&spec, vec![], Some(operation_id.clone())))
        {
            assert_eq!(stats.total_operations, 2);
        } else {
            panic!("LintingOutcome::AllGood expected!")
        }
    }
}
