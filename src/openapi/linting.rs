use indexmap::IndexMap;
use nonempty::NonEmpty;
use openapiv3::{OpenAPI, Operation, PathItem};
use std::fmt::Display;

use crate::{
    examples::{self, need_example, ExamplePayloads},
    Tag,
};

use super::operations::*;
use super::Components;

#[derive(Debug, Default)]
pub(crate) struct Stats {
    pub total_operations: u16,
    pub matching_filters: u16,
    pub operations_needing_examples: u16,
    pub operations_with_invalid_examples: u16,
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
pub(crate) enum LintingIssue {
    ExampleError(examples::ExampleError),
    SchemaNeedsExample(examples::SchemaNeedsExample),
}

impl Display for LintingIssue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LintingIssue::ExampleError(err) => write!(f, "{}", err),
            LintingIssue::SchemaNeedsExample(err) => write!(f, "{}", err),
        }
    }
}

impl From<examples::ExampleError> for LintingIssue {
    fn from(err: examples::ExampleError) -> Self {
        LintingIssue::ExampleError(err)
    }
}

impl From<examples::SchemaNeedsExample> for LintingIssue {
    fn from(err: examples::SchemaNeedsExample) -> Self {
        LintingIssue::SchemaNeedsExample(err)
    }
}

type OperationLintingIssues = IndexMap<OperationContext, NonEmpty<LintingIssue>>;

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

    let path_items: Vec<(String, PathItem)> = super::clone_items(&spec.paths).into_iter().collect();

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
        let operation_report: IndexMap<OperationContext, NonEmpty<LintingIssue>> = operation_report
            .into_iter()
            .map(|(key, errs)| (key, errs.map(From::from)))
            .collect();

        stats.operations_with_invalid_examples += operation_report.len() as u16;

        operation_linting_issues.extend(operation_report);
    }

    if let Err(needing_examples) = needing_example_result {
        stats.operations_needing_examples += needing_examples.len() as u16;

        for (operation_id, schema_needing_example) in needing_examples {
            if let Some(issues) = operation_linting_issues.get_mut(&operation_id) {
                issues.push(schema_needing_example.into());
            } else {
                operation_linting_issues
                    .insert(operation_id, NonEmpty::new(schema_needing_example.into()));
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
