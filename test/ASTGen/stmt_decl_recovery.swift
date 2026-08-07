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

// An empty generic parameter list, which recovery produces for `struct S< {}`.
// ASTGen used to build a GenericParamList with no parameters, tripping
// "Parsed an empty generic parameter list?" downstream; the C++ parser produces
// no list at all.
struct EmptyGenericParams< {}
// expected-astgen-error@-1 {{expected '>' to end generic parameter clause}}
// expected-astgen-note@-2 {{to match this opening '<'}}
// expected-astgen-note@-3 {{insert '>'}}
// expected-cxx-error@-4 {{expected an identifier to name generic parameter}}

// A 'deinit' outside a struct, enum, class, or @_objcImplementation extension.
// ASTGen used to leave the decl valid, and Sema then dereferenced a null
// nominal type.
deinit {}
// expected-error@-1 {{deinitializers may only be declared within a class, actor, or noncopyable type}}

// Only an infix operator may declare a precedence group; passing one through for
// a prefix operator violated an OperatorDecl invariant.
precedencegroup SomeGroup {}
prefix operator +++** : SomeGroup { }
// expected-error@-1 {{only infix operators may declare a precedence}}
// expected-astgen-error@-2 {{operator should not be declared with body}}
// expected-astgen-note@-3 {{remove operator body}}
// expected-cxx-error@-4 {{operator should no longer be declared with body}}

// A repeated precedence group attribute. ASTGen reported this through an ad-hoc
// diagnostic whose precondition required both nodes to have the same syntax
// kind, and passed the wrong node, so the precondition failed.
precedencegroup RepeatedAttribute {
  associativity: left
  associativity: left
  // expected-error@-1 {{'associativity' attribute for precedence group declared multiple times}}
}

// '@nonexhaustive' takes an optional '(warn)'. ASTGen listed it as a simple
// attribute, so building it hit "not a simple attribute".
@nonexhaustive public enum Exhaustive { case a }
@nonexhaustive(warn) public enum ExhaustiveWarn { case a }

// An 'associatedtype' outside a protocol. The AST verifier rejects such a decl,
// so the C++ parser reports the placement and produces none; ASTGen used to
// build one anyway.
associatedtype Stray = Int
// expected-error@-1 {{associated types can only be defined in a protocol; define a type or introduce a 'typealias' to satisfy an associated type requirement}}

protocol HasAssociatedType {
  associatedtype Element
}

// Parameter clause parentheses the parser had to synthesize. Their zero-width
// positions land on whatever follows -- the body's '{' -- so a parameter list
// built from them overlapped the body, which ASTScope rejects. The two parsers
// recover differently here (ASTGen keeps the recovered parameters, the C++
// parser drops them), so each asserts its own diagnostics; both reject.
struct MissingInitParens {
  init c d: Int {}
  // expected-astgen-error@-1 {{expected '(' to start parameter clause}}
  // expected-astgen-note@-2 {{insert '('}}
  // expected-astgen-error@-3 {{expected ')' to end parameter clause}}
  // expected-astgen-note@-4 {{insert ')'}}
  // expected-cxx-error@-5 {{expected '(' for initializer parameters}}
  // expected-cxx-error@-6 {{initializer requires a body}}
}

struct MissingSubscriptParens {
  subscript x y : Int -> Int {
    // expected-astgen-error@-1 {{expected '(' to start parameter clause}}
    // expected-astgen-note@-2 {{insert '('}}
    // expected-astgen-error@-3 {{expected '(' to start function type}}
    // expected-astgen-note@-4 {{insert '('}}
    // expected-astgen-error@-5 {{expected ')' in function type}}
    // expected-astgen-note@-6 {{insert ')'}}
    // expected-astgen-error@-7 {{expected ')' to end parameter clause}}
    // expected-astgen-note@-8 {{insert ')'}}
    // expected-astgen-error@-9 {{expected '->' and return type in subscript}}
    // expected-astgen-note@-10 {{insert '->' and return type}}
    // expected-cxx-error@-11 {{expected '(' for subscript parameters}}
    get { 0 }
  }
}

// The same, but with no parameters at all: reporting no parentheses would leave
// the list with an entirely invalid range, which ASTScope also rejects.
struct EmptySubscriptParams {
  subscript -> Int {
    // expected-astgen-error@-1 {{expected parameter clause in subscript}}
    // expected-astgen-note@-2 {{insert parameter clause}}
    // expected-cxx-error@-3 {{expected '(' for subscript parameters}}
    get { 0 }
  }
}
