// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify
// REQUIRES: objc_interop

import Foundation

@objc protocol P {
  func foo(a arg: Int) // expected-note {{protocol requires function 'foo(a:)' with type '(Int) -> ()'}}
}

class C : P {
// expected-error@-1 {{type 'C' does not conform to protocol 'P'}}
// expected-note@-2 {{add stubs for conformance}}
  var foo: Float = 0.75
  // expected-note@-1 {{'foo' previously declared here}}
  func foo() {}
  // expected-error@-1 {{invalid redeclaration of 'foo()'}}
}
