// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

// REQUIRES: objc_interop

// Invalid expression, used to be slow

import Foundation

func x(_ a: NSArray) {
  let str = String(
    format: "%@,%@,%@,%@,%@,%@,%@",
    String(a.a),  // expected-error {{value of type 'NSArray' has no member 'a'}}
    String(a.b),  // expected-error {{value of type 'NSArray' has no member 'b'}}
    String(a.c),  // expected-error {{value of type 'NSArray' has no member 'c'}}
    String(a.d),  // expected-error {{value of type 'NSArray' has no member 'd'}}
    String(a.e),  // expected-error {{value of type 'NSArray' has no member 'e'}}
    String(a.f),  // expected-error {{value of type 'NSArray' has no member 'f'}}
    String(a.g)   // expected-error {{value of type 'NSArray' has no member 'g'}}
  )
}

