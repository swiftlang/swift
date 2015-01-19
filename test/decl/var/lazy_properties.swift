// RUN: %target-parse-verify-swift -parse-as-library

lazy func lazy_func() {} // expected-error {{'lazy' may only be used on 'var' declarations}}

lazy var b = 42  // expected-error {{'lazy' may not be used on an already-lazy global}}

struct S {
  lazy static var lazy_global = 42 // expected-error {{'lazy' may not be used on an already-lazy global}}
}

protocol SomeProtocol {
  lazy var x : Int  // expected-error {{'lazy' isn't allowed on a protocol requirement}}
  lazy var y : Int { get } // expected-error {{'lazy' isn't allowed on a protocol requirement}}
}


class TestClass {
  lazy var a = 42
  lazy var a1 : Int = 42

  lazy let b = 42  // expected-error {{'lazy' cannot be used on a let}}

  lazy var c : Int { return 42 } // expected-error {{'lazy' may not be used on a computed property}}

  lazy var d : Int  // expected-error {{lazy properties must have an initializer}}

  lazy var (e, f) = (1,2)  // expected-error {{'lazy' cannot destructure an initializer}}

  lazy var g : Int = { 0 }()   // single-expr closure

  lazy var h : Int = { return 0 }()+1  // multi-stmt closure

  lazy var i : Int = 42 {  // expected-error {{lazy properties may not have observers}}
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
    lazy var localvar = 42  // expected-error {{lazy is only valid for members of a struct or class}}
  }
}


struct StructTest {
  lazy var p1 : Int = 42

  mutating func f1() -> Int {
    return p1
  }
  func f2() -> Int {
    return p1  // expected-error {{immutable value of type 'StructTest' only has mutating members named 'p1'}}
  }

  static func testStructInits() {
    let a = StructTest()         // default init
    let b = StructTest(p1: 42)  // Override the lazily init'd value.
  }
}


// <rdar://problem/16889110> capture lists in lazy member properties cannot use self
class CaptureListInLazyProperty {
  lazy var closure: () -> Int = { [weak self] in return self!.i }
  var i = 42
}


