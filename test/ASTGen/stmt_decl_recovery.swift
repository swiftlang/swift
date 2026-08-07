// Statement and declaration forms that used to crash ASTGen.

// RUN: %target-typecheck-verify-swift \
// RUN:   -verify-additional-prefix astgen- \
// RUN:   -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift \
// RUN:   -verify-additional-prefix cxx-

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// An initializer cannot declare a result type. ASTGen used to crash rather than
// report it.
struct HasInit {
  init() -> HasInit { }
  // expected-error@-1 {{initializers cannot have a result type}}
}

// An optional binding condition has to bind an identifier. ASTGen had this
// recovery unimplemented and crashed on both shapes.
struct HasProperty {
  var value: Int?
}

func unwrapMemberAccess(_ x: HasProperty) {
  if let x.value { }
  // expected-error@-1 {{unwrap condition requires a valid identifier}}
}

func unwrapTuplePattern() {
  let a: Int? = nil
  let b: Int? = nil
  if let (a, b) { }
  // expected-error@-1 {{unwrap condition requires a valid identifier}}
  // expected-error@-2 2 {{pattern variable binding cannot appear in an expression}}
}

// A malformed layout constraint in '@_specialize'. The parser reports the
// missing alignment too, so ASTGen sees one more diagnostic than the C++ parser
// does; both reject.
struct Layout<S> {}

@_specialize(where S: _Trivial(64, ))
// expected-error@-1 {{expected non-negative alignment to be specified in layout constraint}}
// expected-astgen-error@-2 {{expected alignment in layout requirement}}
// expected-astgen-note@-3 {{insert alignment}}
public func specialized<S>(_ s: S) {}
