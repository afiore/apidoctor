pub mod linting;
pub mod metadata;
pub mod operations;

use std::{fs::File, io::BufReader, path::Path};

use indexmap::IndexMap;

use openapiv3::{OpenAPI, ReferenceOr, RequestBody, Response, StatusCode};
use serde_json::Value;

use crate::AppError;

pub(crate) fn spec_from_file<P: AsRef<Path>>(path: P) -> Result<OpenAPI, AppError> {
    let file = File::open(path.as_ref())?;
    let reader = BufReader::new(file);
    let extension = path.as_ref().extension().and_then(|ext| ext.to_str());

    let spec = if let Some("yml") | Some("yaml") = extension {
        serde_yaml::from_reader(reader)?
    } else {
        serde_json::from_reader(reader)?
    };
    Ok(spec)
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

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::linting::*;
    use super::operations::*;
    use crate::openapi::*;
    use openapiv3::PathItem;
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
