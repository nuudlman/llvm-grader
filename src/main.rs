use anyhow::Result;

use llvm_grader::{CSrcs, Grader};

fn main() -> Result<()> {
    Grader {
        modifiable_srcs: CSrcs {
            srcs: &["cgen.cpp", "CgenEnvironment.cpp"],
            hdrs: &["CgenEnvironment.h"],
        },
        ..Default::default()
    }
    .run()
}
