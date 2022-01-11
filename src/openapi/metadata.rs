use std::fmt::Display;

use indexmap::IndexMap;
use nonempty::NonEmpty;
use openapiv3::Operation;

use super::operations::{OperationContext, OperationWithContext};

#[derive(Debug, PartialEq)]
pub(crate) enum OperationMetadata {
    SummaryOrDescription,
    Tags,
}

impl Display for OperationMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            OperationMetadata::SummaryOrDescription => "summary/description",
            OperationMetadata::Tags => "tags",
        };
        write!(f, "{}", &s)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct NeedsMetadata(NonEmpty<OperationMetadata>);

impl Display for NeedsMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut metadata: Vec<String> = Vec::new();
        for meta in self.0.iter() {
            metadata.push(format!("{}", meta));
        }
        write!(f, "{}", metadata.join(", "))
    }
}

fn needs_metadata(operation: &Operation) -> Option<NeedsMetadata> {
    let mut missing_metadata = Vec::new();
    if operation.summary.is_none() && operation.description.is_none() {
        missing_metadata.push(OperationMetadata::SummaryOrDescription);
    }
    if operation.tags.len() == 0 {
        missing_metadata.push(OperationMetadata::Tags);
    }

    NonEmpty::from_vec(missing_metadata).map(NeedsMetadata)
}

pub(crate) async fn need_metadata(
    operations: &Vec<OperationWithContext>,
) -> Result<(), IndexMap<OperationContext, NeedsMetadata>> {
    let mut operation_errors = IndexMap::new();

    for op in operations.iter() {
        if let Some(needs_meta) = needs_metadata(&op.operation) {
            operation_errors.insert(op.context.clone(), needs_meta);
        }
    }
    if operation_errors.len() == 0 {
        Ok(())
    } else {
        Err(operation_errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use openapiv3::Operation;

    #[test]
    fn empty_operation_needs_all_metadata() {
        let operation = Operation::default();
        let result = needs_metadata(&operation);
        let all_metadata = NonEmpty::from_vec(vec![
            OperationMetadata::SummaryOrDescription,
            OperationMetadata::Tags,
        ])
        .unwrap();
        assert_eq!(result, Some(NeedsMetadata(all_metadata)));
    }

    #[test]
    fn op_with_tags_and_description_or_summary_is_ok() {
        let mut operation = Operation::default();
        operation.tags = vec!["a".to_owned()];

        let mut with_description = operation.clone();
        with_description.description = Some("blurb".to_owned());

        let mut with_summary = operation.clone();
        with_summary.summary = Some("blurb".to_owned());

        let mut with_both = with_summary.clone();
        with_both.description = Some("burb".to_owned());

        for operation in vec![with_description, with_summary, with_both] {
            assert_eq!(needs_metadata(&operation), None);
        }
    }
}
