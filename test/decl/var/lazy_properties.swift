// RUN: %target-typecheck-verify-swift -parse-as-library

lazy func lazy_func() {} // expected-error {{'lazy' may only be used on 'var' declarations}} {{1-6=}}

lazy var b = 42  // expected-error {{'lazy' may not be used on an already-lazy global}} {{1-6=}}

struct S {
  lazy static var lazy_global = 42 // expected-error {{'lazy' may not be used on an already-lazy global}} {{3-8=}}
}

protocol SomeProtocol {
  lazy var x : Int  // expected-error {{'lazy' isn't allowed on a protocol requirement}} {{3-8=}}
  // expected-error@-1 {{property in protocol must have explicit { get } or { get set } specifier}}
  lazy var y : Int { get } // expected-error {{'lazy' isn't allowed on a protocol requirement}} {{3-8=}}
}


class TestClass {
  lazy var a = 42
  lazy var a1 : Int = 42

  lazy let b = 42  // expected-error {{'lazy' cannot be used on a let}} {{3-8=}}

  lazy var c : Int { return 42 } // expected-error {{'lazy' may not be used on a computed property}} {{3-8=}}

  lazy var d : Int  // expected-error {{lazy properties must have an initializer}} {{3-8=}}

  lazy var (e, f) = (1,2)  // expected-error {{'lazy' cannot destructure an initializer}} {{3-8=}}

  lazy var g : Int = { 0 }()   // single-expr closure

  lazy var h : Int = { return 0 }()+1  // multi-stmt closure

  lazy var i : Int = 42 {  // expected-error {{lazy properties may not have observers}} {{3-8=}}
    didSet {
    }
  }

  // Lazy values can have observers, be NSCopying, etc.
/*  lazy var d : Int = 42 {
    didSet {
      print("set me")
    }
  }*/

  init() {
    lazy var localvar = 42  // expected-error {{lazy is only valid for members of a struct or class}} {{5-10=}}
    localvar += 1
    _ = localvar
  }
}


struct StructTest {
  lazy var p1 : Int = 42

  mutating func f1() -> Int {
    return p1
  }
  
  // expected-note @+1 {{mark method 'mutating' to make 'self' mutable}} {{3-3=mutating }}
  func f2() -> Int {
    return p1  // expected-error {{cannot use mutating getter on immutable value: 'self' is immutable}}
  }

  static func testStructInits() {
    let a = StructTest()         // default init
    let b = StructTest(p1: 42)  // Override the lazily init'd value.
    _ = a; _ = b
  }
}


// <rdar://problem/16889110> capture lists in lazy member properties cannot use self
class CaptureListInLazyProperty {
  lazy var closure: () -> Int = { [weak self] in return self!.i }
  var i = 42
}


// Crash when initializer expression is type-checked both to infer the
// property type and also as part of the getter
class WeShouldNotReTypeCheckStatements {
  lazy var firstCase = {
    _ = nil // expected-error {{'nil' requires a contextual type}}
    _ = ()
  }

  lazy var secondCase = {
    _ = ()
    _ = ()
  }
}
