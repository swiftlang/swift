// Clauses after '#else' are unreachable, which both parsers diagnose.

// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

struct S {
  #if true
  #else
  #else  // expected-error {{further conditions after #else are unreachable}}
  #endif
}

#if true
func f() {}
#else
#elseif true  // expected-error {{further conditions after #else are unreachable}}
#endif
