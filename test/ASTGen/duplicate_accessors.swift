// A second accessor of a kind that already appeared. ASTGen did not report it,
// which let an invalid AST through to the verifier: getAccessor() returns only
// the first accessor of a kind, so the second failed the verifier's
// "storage declaration has different accessor for this kind" check -- and that
// check only runs when no error has been emitted.

// RUN: %target-typecheck-verify-swift -parse-as-library \
// RUN:   -enable-experimental-feature CoroutineAccessors \
// RUN:   -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift -parse-as-library \
// RUN:   -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_CoroutineAccessors

var twoGetters: Int {
  get { 0 }
  get { 1 } // expected-error {{variable already has a getter}}
  // expected-note@-2 {{previous definition of getter here}}
}

var twoSetters: Int {
  get { 0 }
  set { }
  set { } // expected-error {{variable already has a setter}}
  // expected-note@-2 {{previous definition of setter here}}
}

// Every duplicate past the first is reported, each against the first.
var threeGetters: Int {
  get { 0 }
  get { 1 } // expected-error {{variable already has a getter}}
  get { 2 } // expected-error {{variable already has a getter}}
  // expected-note@-3 2 {{previous definition of getter here}}
}

struct WithSubscript {
  // The wording distinguishes a subscript from a variable.
  subscript(i: Int) -> Int {
    get { 0 }
    get { 1 } // expected-error {{subscript already has a getter}}
    // expected-note@-2 {{previous definition of getter here}}
  }
}

// 'read' is the deprecated spelling of 'yielding borrow', so these are the same
// accessor kind and the second one is a duplicate rather than a conflict.
var readAndYieldingBorrow: Int {
  read { // expected-note {{previous definition of 'yielding borrow' accessor here}}
    // expected-warning@-1 {{'read' is deprecated; please use 'yielding borrow' instead}}
    yield 0
  }
  yielding borrow { // expected-error {{variable already has a 'yielding borrow' accessor}}
    yield 0
  }
}
