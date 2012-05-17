// RUN: %swift %s -verify

extension extension_for_invalid_type {  // expected-error {{use of undeclared type}}
  static func f() { }
}

typealias Point : (x : Int, y : Int)

extension Point { } // expected-error{{non-nominal type 'Point' cannot be extended}}