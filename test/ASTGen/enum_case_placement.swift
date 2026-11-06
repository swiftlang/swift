// An enum 'case' outside of an enum is diagnosed by both parsers.

// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

struct S {
  case A // expected-error {{enum 'case' is not allowed outside of an enum}}
}

class C {
  case B, D // expected-error {{enum 'case' is not allowed outside of an enum}}
}

enum E {
  case X
#if true
  case Y
#endif
}
