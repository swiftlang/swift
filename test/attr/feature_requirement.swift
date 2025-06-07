// RUN: %target-typecheck-verify-swift -parse-as-library -disable-experimental-parser-round-trip -verify-additional-prefix disabled-
// RUN: %target-typecheck-verify-swift -parse-as-library -verify-additional-prefix enabled- -enable-experimental-feature ExtensibleAttribute

// REQUIRES: asserts

// This test checks whether DECL_ATTR_FEATURE_REQUIREMENT is being applied correctly.
// It is expected to need occasional edits as experimental features are stabilized.

@extensible
public enum E {}  // expected-disabled-error@-1 {{'extensible' attribute is only valid when experimental feature ExtensibleAttribute is enabled}}

#if hasAttribute(extensible)
  #error("does have @extensible")  // expected-enabled-error {{does have @extensible}}
#else
  #error("doesn't have @extensible")  // expected-disabled-error {{doesn't have @extensible}}
#endif

