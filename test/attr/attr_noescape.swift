// RUN: %swift -parse %s -verify

@__noescape var fn : () -> Int = { 4 }  // expected-error {{'__noescape' may only be used on 'parameter' declarations}}

func takesClosure(@__noescape fn : () -> Int) {
  takesClosure { 4 }  // ok
}

class SomeClass {
  final var x = 42

  func test() {
    // Since 'takesClosure' doesn't escape its closure, it doesn't require
    // "self." qualification of member references.
    takesClosure { x }
  }


}
