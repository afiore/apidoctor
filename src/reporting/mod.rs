use std::fmt::Display;

use crate::examples::ErrorReport;

struct ConsolePrinter {
    _report: ErrorReport,
}

impl Display for ConsolePrinter {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
