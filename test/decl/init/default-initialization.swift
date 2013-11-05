// RUN: %swift %s -verify

struct A {
  var i : Int
  init(i : Int) { self.i = i }
}

struct B {
  var a : A // expected-note{{'a' declared here}} expected-note{{'a' declared here}}
}

def locals() {
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
  init() { // expected-error{{cannot default-initialize instance variable 'a' of type 'A'}}
  }

  init(inA : A) { // okay
    a = inA
  }

  init(otherA : A) { // okay
    self.a = otherA
  }

  init(i : Int) { // okay
    a = A(i)
  }

  init(a : A) {
    self.a = a 
  }

  // FIXME: False positive. We're actually definitively initializing self.
  init(j : Int) { // expected-error{{cannot default-initialize instance variable 'a' of type 'A'}}
    if true { a = A(j) }
  }

  // FIXME: False negative. We're reading before we're writing.
  init(i : Int) {
    a = A(a.i)
  }

  // Initializing the whole struct at once.
  init(k : Int) {
    var b : B
    self = b
  }
}

struct D {
  var (a1, a2) : (A, A) = (A(1), A(2))
}

var d : D // okay; uses initializer provided for a1, a2

protocol P { }
var p : P

// Properties don't need to be default-initializable.
var prop : P {
get:
  return p
}

var prop2 : (P) {
get:
  return p
}

class Base { }
class Derived : Base { }

class NoInitBase {
  init(x : Int) { }
}

class NoInitDerived : NoInitBase { }

Base()
Derived()
NoInitBase() // expected-error{{does not type-check}}
NoInitDerived() // expected-error{{'NoInitDerived' is not constructible with '()'}}
