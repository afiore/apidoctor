use std::fmt::Display;

use openapiv3::{Operation, PathItem};

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
