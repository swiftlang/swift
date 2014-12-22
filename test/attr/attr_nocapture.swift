// RUN: %swift -parse %s -verify

@__nocapture var fn : () -> Int = { 4 }  // expected-error {{'__nocapture' may only be used on 'parameter' declarations}}

func f(@__nocapture fn : () -> Int) {
  f { 4 }  // ok
}

class SomemClass {
  var x = 42

  // TODO: We should be able to eliminate this.
  func test() {
    f { x }       // expected-error {{reference to property 'x' in closure requires explicit 'self.' to make capture semantics explicit}}
  }


}
