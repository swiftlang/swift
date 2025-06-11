// RUN: %target-typecheck-verify-swift -parse-as-library -disable-experimental-parser-round-trip -verify-additional-prefix disabled-
// RUN: %target-typecheck-verify-swift -parse-as-library -verify-additional-prefix enabled- -enable-experimental-feature CompileTimeValues

// REQUIRES: asserts

// This test checks whether DECL_ATTR_FEATURE_REQUIREMENT is being applied correctly.
// It is expected to need occasional edits as experimental features are stabilized.

@const
public let x = 1  // expected-disabled-error@-1 {{'const' attribute is only valid when experimental feature CompileTimeValues is enabled}}

#if hasAttribute(const)
  #error("does have @const")  // expected-enabled-error {{does have @const}}
#else
  #error("doesn't have @const")  // expected-disabled-error {{doesn't have @const}}
#endif
