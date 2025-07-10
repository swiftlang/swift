// This contains code completion test cases for features covered by experimental
// feature flags, and tests both the case when the feature is disabled and when
// it's enabled. When a feature becomes non-experimental, move its test cases
// into the normal complete_decl_attribute.swift test file.

// REQUIRES: swift_feature_NonexhaustiveAttribute

// RUN: %batch-code-completion -filecheck-additional-suffix _DISABLED
// RUN: %batch-code-completion -filecheck-additional-suffix _ENABLED \
// RUN:        -enable-experimental-feature NonexhaustiveAttribute

// NOTE: Please do not include the ", N items" after "Begin completions". The
// item count creates needless merge conflicts given that an "End completions"
// line exists for each test.

@#^KEYWORD4^# enum E {}
// KEYWORD4:              Begin completions
// KEYWORD4_ENABLED-DAG:  Keyword/None:              nonexhaustive[#{{.*}} Attribute#]; name=nonexhaustive
// KEYWORD4_DISABLED-NOT: Keyword/None:              nonexhaustive[#{{.*}} Attribute#]; name=nonexhaustive
// KEYWORD4:              End completions
