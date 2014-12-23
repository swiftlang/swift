// RUN: %swift -parse %s -verify

@__noescape var fn : () -> Int = { 4 }  // expected-error {{'__noescape' may only be used on 'parameter' declarations}}

func doesEscape(fn : () -> Int) {}

func takesGenericClosure<T>(a : Int, @__noescape fn : () -> T) {}


func takesNoEscapeClosure(@__noescape fn : () -> Int) {
  takesNoEscapeClosure { 4 }  // ok

  fn()  // ok

  var x = fn  // expected-error {{@noescape argument 'fn' may only be called}}

  // This is ok, because the closure itself is noescape.
  takesNoEscapeClosure { fn() }

  // This is not ok, because it escapes the 'fn' closure.
  doesEscape { fn() }   // expected-error {{closure use of @noescape argument 'fn' may allow it to escape}}

  // This is not ok, because it escapes the 'fn' closure.
  func nested_function() {
    fn()   // expected-error {{declaration closing over @noescape argument 'fn' may allow it to escape}}
  }

  takesNoEscapeClosure(fn)  // ok

  doesEscape(fn)                   // expected-error {{invalid use of non-escaping function in escaping context '() -> Int'}}
  takesGenericClosure(4, fn)       // ok
  takesGenericClosure(4) { fn() }  // ok.
}

class SomeClass {
  final var x = 42

  func test() {
    // This should require "self."
    doesEscape { x }  // expected-error {{reference to property 'x' in closure requires explicit 'self.' to make capture semantics explicit}}

    // Since 'takesClosure' doesn't escape its closure, it doesn't require
    // "self." qualification of member references.
    takesNoEscapeClosure { x }
  }
}
