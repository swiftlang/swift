// RUN: %target-swift-frontend -emit-sil -verify %s

// REQUIRES: swift_in_compiler

struct A {
  var i : Int
  init(i : Int) { self.i = i }
}

struct B {
  var a : A  // expected-note 3 {{'self.a' not initialized}}
}

func locals() {
  var al : A
  var bl : B
  
  al = A(i: 1)
  bl = B(a: al)
  _ = bl
}

var ag : A
var bg : B

struct C {
  var x : (Int, Int)
}

var c : C


extension B {
  init() {
    // The usage is that self.a is returned from init() as part of self.
  } // expected-error {{return from initializer without initializing all stored properties}}

  init(inA : A) { // okay
    a = inA
  }

  init(otherA : A, x : Bool) { // okay
    self.a = otherA
  }

  init(i : Int) { // okay
    a = A(i: i)
  }

  init(a : A, x : Bool, y : Bool) {
    self.a = a
  }

  init(j : Int, x : Bool) {
    if 1 == 1 { a = A(i: j) }
  } // expected-error {{return from initializer without initializing all stored properties}}

  init(j : Int, x2 : Bool) {
    if true { a = A(i: j) }
  }

  init(i : Int, x : Bool, y : Bool) {
    a = A(i: a.i)    // expected-error {{'self' used before all stored properties are initialized}}
  }

  // Initializing the whole struct at once.
  init(k : Int, x : Bool, y : Bool, z : Bool) {
    let b : B     // expected-note {{constant defined here}}
    self = b      // expected-error {{constant 'b' used before being initialized}}
  }
}

struct D {
  var (a1, a2) : (A, A) = (A(i: 1), A(i: 2))
}

var d : D // okay; uses initializer provided for a1, a2

protocol P { }
struct PImpl : P {}
var p : P = PImpl()

// Properties don't need to be default-initializable.
var prop : P {
  get {
    return p
  }
}

var prop2 : (P) {
  get {
    return p
  }
}

class Base { }
class Derived : Base { }

class NoInitBase {
  init(x : Int) { }
}

class NoInitDerived : NoInitBase { }

Base() // expected-warning{{unused}}
Derived() // expected-warning{{unused}}

class MultipleInitBase {
  init() { }
  init(i: Int) { }
}

class MultipleInitDerived : MultipleInitBase {
  override init() { } // expected-error{{'super.init' isn't called on all paths before returning from initializer}}
}

// rdar://20944100
// Include a 'try' when synthesizing an override of a throwing constructor.
public class HasThrowingInit {
  public init() throws { }
}
public class AlsoHasThrowingInit : HasThrowingInit {
  public convenience init(flag: Bool) throws {
    try self.init()
  }

  public convenience init(flag2: Bool) {
    try! self.init()
  }
}
