use std::{convert::Infallible, fmt::Display, str::FromStr};

use nonempty::NonEmpty;
use openapiv3::{Operation, PathItem};

use crate::Tag;

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
    pub(super) fn for_method_and_path(method: &HttpMethod, path: &str) -> Self {
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

pub(super) struct OperationIterator {
    path_items: Vec<(String, PathItem)>,
    current_item_operations: Option<NonEmpty<OperationWithContext>>,
    current_item: usize,
}

impl OperationIterator {
    pub(super) fn new(path_items: Vec<(String, PathItem)>) -> Self {
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
