// Checks the C++ parser makes while collecting a declaration's accessors and
// its parameters, checked against both parsers.

// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

struct S {
  var onlySetter: Int { set {} }
  // expected-error@-1 {{variable with a setter must also have a getter}}

  var conflicting: Int {
    get { 0 }
    unsafeAddress { fatalError() }
    // expected-error@-1 {{variable cannot provide both an addressor and a getter}}
    // expected-note@-3 {{getter defined here}}
  }

  subscript(i: Int) -> Int {
    get { 0 }
    willSet {}
    // expected-error@-1 {{'willSet' is not allowed in subscripts}}
  }

  var effectfulGet: Int {
    set {} // expected-error {{'set' accessor is not allowed on property with 'get' accessor that is 'async' or 'throws'}}
    get throws { 0 }
  }
}

let computedLet: Int
{ 1 } // expected-error {{'let' declarations cannot be computed properties}}

protocol P {
  func method(_ truth: Bool = false)
  // expected-error@-1 {{default argument not permitted in a protocol method}}
  init(truth: Bool = false)
  // expected-error@-1 {{default argument not permitted in a protocol initializer}}
  subscript(x: Int, default: Int = 0) -> Self { get }
  // expected-error@-1 {{default argument not permitted in a protocol subscript}}
}
