// This contains code completion test cases for features covered by experimental
// feature flags, and tests both the case when the feature is disabled and when
// it's enabled. When a feature becomes non-experimental, move its test cases
// into the normal complete_decl_attribute.swift test file.

// RUN: %batch-code-completion -filecheck-additional-suffix _DISABLED
// RUN: %batch-code-completion -filecheck-additional-suffix _ENABLED

// NOTE: There are currently no experimental features that need code completion
// testing, but this test file is being left in place for when it's needed
// again. At that time, please remove the ABIAttribute tests.
// REQUIRES: new_use_case

// NOTE: Please do not include the ", N items" after "Begin completions". The
// item count creates needless merge conflicts given that an "End completions"
// line exists for each test.

