// RUN: %target-typecheck-verify-swift -parse-as-library -disable-experimental-parser-round-trip -verify-additional-prefix disabled-
// RUN: %target-typecheck-verify-swift -parse-as-library -verify-additional-prefix enabled- -enable-experimental-feature NonexhaustiveAttribute

// REQUIRES: asserts

// This test checks whether DECL_ATTR_FEATURE_REQUIREMENT is being applied correctly.
// It is expected to need occasional edits as experimental features are stabilized.

@nonexhaustive
public enum E {}  // expected-disabled-error@-1 {{'nonexhaustive' attribute is only valid when experimental feature NonexhaustiveAttribute is enabled}}

#if hasAttribute(nonexhaustive)
  #error("does have @nonexhaustive")  // expected-enabled-error {{does have @nonexhaustive}}
#else
  #error("doesn't have @nonexhaustive")  // expected-disabled-error {{doesn't have @nonexhaustive}}
#endif

