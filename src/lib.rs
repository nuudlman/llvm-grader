//! # Gradescope Grader
//!
//! Complies with the [gradescope spec](https://gradescope-autograders.readthedocs.io/en/latest/specs/)
//! - `/autograder/submission` contains the student's submission, downloaded from Gradescope.
//! - `/autograder/handout` contains the lab handout (copied into the Docker image)
//! - `/autograder/results/results.json` contains the student's grade produced by running this grader
//!
//!
//! ## Design
//! 1. Create a working directory from the handout.
//! 2. Merge the student's submission with the working directory
//! (in essence merging the handout and the submission).
//! The list of files the student can modify must be provided.
//! 3. Configure the CMake build directory
//! 4. Build executable
//! 5. Glob all test files, and run the tests, calculating a pass/fail for each one
//! 6. Calculate the total grade
//! 7. Emit the grade report
//!
//! If the build steps fail, the student gets a grade of zero
//! (but we emit the relevant output so that the student can debug).

use anyhow::Result;
use std::marker::PhantomData;
use std::path::PathBuf;
use std::process::{Command, Output};

#[derive(Debug, Default)]
pub struct CSrcs {
    pub srcs: &'static [&'static str],
    pub hdrs: &'static [&'static str],
}

/// Configure the grader's options.
#[derive(Debug)]
pub struct Grader {
    pub modifiable_srcs: CSrcs,
    pub show_diff: bool,
    pub timeout: u64,
    pub configure_options: &'static str,
    pub build_options: &'static str,
    pub run_options: &'static str,
    pub test_runner:
        fn(exe_file: &PathBuf, test_file: &PathBuf, working_directory: &PathBuf) -> Result<Output>,
}

impl Default for Grader {
    fn default() -> Self {
        Grader {
            modifiable_srcs: CSrcs::default(),
            show_diff: true,
            timeout: 60,
            configure_options: "",
            build_options: "",
            run_options: "",
            test_runner: |exe_file, test_file, _working_directory| {
                let output = Command::new(exe_file).arg(test_file).output()?;
                Ok(output)
            },
        }
    }
}

#[derive(Debug, Default)]
struct GradeCtx<S: GraderState> {
    marker: PhantomData<S>,
    config: Grader,
}

struct Start;
struct InitializedWorkingDirectory;
struct MergedStudentSubmission;
struct ConfiguredBuildDirectory;
struct TestExeBuilt;
struct TestsRun;

trait GraderState {}
impl GraderState for Start {}
impl GraderState for InitializedWorkingDirectory {}
impl GraderState for MergedStudentSubmission {}
impl GraderState for ConfiguredBuildDirectory {}
impl GraderState for TestExeBuilt {}
impl GraderState for TestsRun {}

impl Grader {
    pub fn run(self) -> Result<()> {
        GradeCtx::new(self)
            .create_working_directory_from_handout()?
            .merge_submission_with_working_directory()?
            .configure_build()?
            .build_exe()?
            .run_tests()?
            .emit_grade()
    }
}
impl GradeCtx<Start> {
    fn new(config: Grader) -> Self {
        Self {
            marker: PhantomData,
            config,
        }
    }
    fn create_working_directory_from_handout(
        self,
    ) -> Result<GradeCtx<InitializedWorkingDirectory>> {
        Ok(GradeCtx::<InitializedWorkingDirectory> {
            marker: PhantomData,
            config: self.config,
        })
    }
}
impl GradeCtx<InitializedWorkingDirectory> {
    fn merge_submission_with_working_directory(
        self,
    ) -> anyhow::Result<GradeCtx<MergedStudentSubmission>> {
        Ok(GradeCtx::<MergedStudentSubmission> {
            marker: PhantomData,
            config: self.config,
        })
    }
}

impl GradeCtx<MergedStudentSubmission> {
    fn configure_build(self) -> Result<GradeCtx<ConfiguredBuildDirectory>> {
        Ok(GradeCtx::<ConfiguredBuildDirectory> {
            marker: PhantomData,
            config: self.config,
        })
    }
}

impl GradeCtx<ConfiguredBuildDirectory> {
    fn build_exe(self) -> Result<GradeCtx<TestExeBuilt>> {
        Ok(GradeCtx::<TestExeBuilt> {
            marker: PhantomData,
            config: self.config,
        })
    }
}

impl GradeCtx<TestExeBuilt> {
    fn run_tests(self) -> Result<GradeCtx<TestsRun>> {
        Ok(GradeCtx::<TestsRun> {
            marker: PhantomData,
            config: self.config,
        })
    }
}

impl GradeCtx<TestsRun> {
    fn emit_grade(self) -> Result<()> {
        Ok(())
    }
}
