// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

// REQUIRES: objc_interop

// Invalid expression, used to be slow

import Foundation

func x(_ a: NSArray) {
  let str = String(  // expected-error {{reasonable time}}
    format: "%@,%@,%@,%@,%@,%@,%@",
    String(a.a),  // e/xpected-error {{value of type 'NSArray' has no member 'a'}}
    String(a.b),  // e/xpected-error {{value of type 'NSArray' has no member 'b'}}
    String(a.c),  // e/xpected-error {{value of type 'NSArray' has no member 'c'}}
    String(a.d),  // e/xpected-error {{value of type 'NSArray' has no member 'd'}}
    String(a.e),  // e/xpected-error {{value of type 'NSArray' has no member 'e'}}
    String(a.f),  // e/xpected-error {{value of type 'NSArray' has no member 'f'}}
    String(a.g)   // e/xpected-error {{value of type 'NSArray' has no member 'g'}}
  )
}

