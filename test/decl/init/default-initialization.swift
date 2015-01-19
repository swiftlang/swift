// RUN: %target-swift-frontend -emit-sil -verify %s

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
    if true { a = A(i: j) }
  } // expected-error {{return from initializer without initializing all stored properties}}

  init(i : Int, x : Bool, y : Bool) {
    a = A(i: a.i)    // expected-error {{'self' used before all stored properties are initialized}}
  }

  // Initializing the whole struct at once.
  init(k : Int, x : Bool, y : Bool, z : Bool) {
    var b : B     // expected-note {{variable defined here}}
    self = b      // expected-error {{variable 'b' used before being initialized}}
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

Base()
Derived()

class MultipleInitBase {
  init() { }
  init(i: Int) { }
}

class MultipleInitDerived : MultipleInitBase {
  override init() { } // expected-error{{super.init isn't called before returning from initializer}}
}

