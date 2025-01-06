// RUN: %target-typecheck-verify-swift -parse-as-library -disable-experimental-parser-round-trip -verify-additional-prefix disabled-
// RUN: %target-typecheck-verify-swift -parse-as-library -verify-additional-prefix enabled- -enable-experimental-feature ABIAttribute

// REQUIRES: asserts

// This test checks whether DECL_ATTR_FEATURE_REQUIREMENT is being applied correctly.
// It is expected to need occasional edits as experimental features are stabilized.

@abi(func fn())
func fn() {}  // expected-disabled-error@-1 {{'abi' attribute is only valid when experimental feature ABIAttribute is enabled}}

#if hasAttribute(abi)
  #error("does have @abi")  // expected-enabled-error {{does have @abi}}
#else
  #error("doesn't have @abi")  // expected-disabled-error {{doesn't have @abi}}
#endif
