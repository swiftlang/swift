// RUN: %target-parse-verify-swift

@noescape var fn : () -> Int = { 4 }  // expected-error {{@noescape may only be used on 'parameter' declarations}}

func doesEscape(fn : () -> Int) {}

func takesGenericClosure<T>(a : Int, @noescape _ fn : () -> T) {}


func takesNoEscapeClosure(@noescape fn : () -> Int) {
  takesNoEscapeClosure { 4 }  // ok

  fn()  // ok

  var x = fn  // expected-error {{@noescape parameter 'fn' may only be called}}

  // This is ok, because the closure itself is noescape.
  takesNoEscapeClosure { fn() }

  // This is not ok, because it escapes the 'fn' closure.
  doesEscape { fn() }   // expected-error {{closure use of @noescape parameter 'fn' may allow it to escape}}

  // This is not ok, because it escapes the 'fn' closure.
  func nested_function() {
    fn()   // expected-error {{declaration closing over @noescape parameter 'fn' may allow it to escape}}
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

    // Since 'takesNoEscapeClosure' doesn't escape its closure, it doesn't
    // require "self." qualification of member references.
    takesNoEscapeClosure { x }
  }

  func foo() -> Int {
    foo()

    func plain() { foo() }
    let plain2 = { foo() } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}

    func multi() -> Int { foo(); return 0 }
    let mulit2: () -> Int = { foo(); return 0 } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}

    doesEscape { foo() } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
    takesNoEscapeClosure { foo() } // okay

    doesEscape { foo(); return 0 } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
    takesNoEscapeClosure { foo(); return 0 } // okay

    func outer() {
      func inner() { foo() }
      let inner2 = { foo() } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      func multi() -> Int { foo(); return 0 }
      let mulit2: () -> Int = { foo(); return 0 } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      doesEscape { foo() } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      takesNoEscapeClosure { foo() }
      doesEscape { foo(); return 0 } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      takesNoEscapeClosure { foo(); return 0 }
    }

    let outer2: () -> Void = {
      func inner() { foo() } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      let inner2 = { foo() } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      func multi() -> Int { foo(); return 0 } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      let mulit2: () -> Int = { foo(); return 0 } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      doesEscape { foo() }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      takesNoEscapeClosure { foo() }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      doesEscape { foo(); return 0 }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      takesNoEscapeClosure { foo(); return 0 }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
    }

    doesEscape {
      func inner() { foo() }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      let inner2 = { foo() }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      func multi() -> Int { foo(); return 0 }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      let mulit2: () -> Int = { foo(); return 0 }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      doesEscape { foo() }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      takesNoEscapeClosure { foo() }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      doesEscape { foo(); return 0 }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      takesNoEscapeClosure { foo(); return 0 }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
    }
    takesNoEscapeClosure {
      func inner() { foo() }
      let inner2 = { foo() }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      func multi() -> Int { foo(); return 0 }
      let mulit2: () -> Int = { foo(); return 0 }  // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      doesEscape { foo() } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      takesNoEscapeClosure { foo() } // okay
      doesEscape { foo(); return 0 } // expected-error {{call to method 'foo' in closure requires explicit 'self.' to make capture semantics explicit}}
      takesNoEscapeClosure { foo(); return 0 } // okay
    }

    struct Outer {
      func bar() -> Int {
        bar()

        func plain() { bar() }
        let plain2 = { bar() } // expected-error {{call to method 'bar' in closure requires explicit 'self.' to make capture semantics explicit}}

        func multi() -> Int { bar(); return 0 }
        let mulit2: () -> Int = { bar(); return 0 } // expected-error {{call to method 'bar' in closure requires explicit 'self.' to make capture semantics explicit}}

        doesEscape { bar() } // expected-error {{call to method 'bar' in closure requires explicit 'self.' to make capture semantics explicit}}
        takesNoEscapeClosure { bar() } // okay

        doesEscape { bar(); return 0 } // expected-error {{call to method 'bar' in closure requires explicit 'self.' to make capture semantics explicit}}
        takesNoEscapeClosure { bar(); return 0 } // okay

        return 0
      }
    }

    func structOuter() {
      struct Inner {
        func bar() -> Int {
          bar() // no-warning

          func plain() { bar() }
          let plain2 = { bar() } // expected-error {{call to method 'bar' in closure requires explicit 'self.' to make capture semantics explicit}}

          func multi() -> Int { bar(); return 0 }
          let mulit2: () -> Int = { bar(); return 0 } // expected-error {{call to method 'bar' in closure requires explicit 'self.' to make capture semantics explicit}}

          doesEscape { bar() } // expected-error {{call to method 'bar' in closure requires explicit 'self.' to make capture semantics explicit}}
          takesNoEscapeClosure { bar() } // okay

          doesEscape { bar(); return 0 } // expected-error {{call to method 'bar' in closure requires explicit 'self.' to make capture semantics explicit}}
          takesNoEscapeClosure { bar(); return 0 } // okay

          return 0
        }
      }
    }

    return 0
  }
}


// Implicit conversions (in this case to @convention(block)) are ok.
@asmname("whatever")
func takeNoEscapeAsObjCBlock(@noescape @convention(block) () -> Void)
func takeNoEscapeTest2(@noescape fn : () -> ()) {
  takeNoEscapeAsObjCBlock(fn)
}

// Autoclosure implies noescape, but produce nice diagnostics so people know
// why noescape problems happen.
func testAutoclosure(@autoclosure a : () -> Int) { // expected-note{{parameter 'a' is implicitly @noescape because it was declared @autoclosure}}
  doesEscape { a() }  // expected-error {{closure use of @noescape parameter 'a' may allow it to escape}}
}


// <rdar://problem/19470858> QoI: @autoclosure implies @noescape, so you shouldn't be allowed to specify both
func redundant(@noescape  // expected-error {{@noescape is implied by @autoclosure and should not be redundantly specified}}
               @autoclosure fn : () -> Int) {
}


protocol P1 {
  typealias Element
}
protocol P2 : P1 {
  typealias Element
}

func overloadedEach<O: P1, T>(source: O, _ transform: O.Element -> ()) {}

func overloadedEach<P: P2, T>(source: P, _ transform: P.Element -> ()) {}

struct S : P2 {
  typealias Element = Int
  func each(@noescape transform: Int -> ()) {
    overloadedEach(self, transform) // expected-error {{invalid use of non-escaping function in escaping context 'O.Element -> ()'}}
      // expected-error@-1 {{invalid use of non-escaping function in escaping context 'P.Element -> ()'}}
      // expected-error@-2 {{cannot find an overload for 'overloadedEach' that accepts an argument list of type '(S, @noescape Int -> ())'}}
  }
}



// rdar://19763676 - False positive in @noescape analysis triggered by parameter label
func r19763676Callee(@noescape f: (param: Int) -> Int) {}

func r19763676Caller(@noescape g: (Int) -> Int) {
  r19763676Callee({ _ in g(1) })
}


// <rdar://problem/19763732> False positive in @noescape analysis triggered by default arguments
func calleeWithDefaultParameters(@noescape f: () -> (), x : Int = 1) {}  // expected-warning {{closure parameter prior to parameters with default arguments will not be treated as a trailing closure}}

func callerOfDefaultParams(@noescape g: () -> ()) {
  calleeWithDefaultParameters(g)
}



// <rdar://problem/19773562> Closures executed immediately { like this }() are not automatically @noescape
class NoEscapeImmediatelyApplied {
  func f() {
    // Shouldn't require "self.", the closure is obviously @noescape.
    let x = { return ivar }()
  }
  
  final var ivar  = 42
}


