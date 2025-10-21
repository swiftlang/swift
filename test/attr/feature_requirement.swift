// RUN: %target-typecheck-verify-swift -parse-as-library -disable-experimental-parser-round-trip -verify-additional-prefix disabled-
// RUN: %target-typecheck-verify-swift -parse-as-library -verify-additional-prefix enabled-

// REQUIRES: asserts

// NOTE: There are currently no experimental features that need code completion
// testing, but this test file is being left in place for when it's needed
// again. At that time, please remove the ABIAttribute tests.
// REQUIRES: new_use_case

// This test checks whether DECL_ATTR_FEATURE_REQUIREMENT is being applied correctly.
// It is expected to need occasional edits as experimental features are stabilized.

