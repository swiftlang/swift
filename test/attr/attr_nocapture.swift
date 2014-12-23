// RUN: %swift -parse %s -verify

@__noescape var fn : () -> Int = { 4 }  // expected-error {{'__noescape' may only be used on 'parameter' declarations}}

func f(@__noescape fn : () -> Int) {
  f { 4 }  // ok
}

class SomeClass {
  final var x = 42

  // TODO: We should be able to eliminate this.
  func test() {
    f { x }       // expected-error {{reference to property 'x' in closure requires explicit 'self.' to make capture semantics explicit}}
  }


}
