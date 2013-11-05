// RUN: %swift %s -verify

// User-written default constructor
struct X {
  init() {}
}

X()

// User-written memberwise constructor
struct Y {
  var i : Int, f : Float
  init(i : Int, f : Float) {}
}

Y(1, 1.5)

// User-written memberwise constructor with default
struct Z {
  var a : Int
  var b : Int

  init(a : Int, b : Int = 5) {
    self.a = a
    self.b = b
  }
}

Z(1, 2)

// User-written init suppresses implicit constructors.
struct A {
  var i, j : Int
  
  init(x : Int) {
    i = x
    j = x
  }
}

A() // expected-error{{expression does not type-check}}
A(1)
A(1, 1) // expected-error{{expression does not type-check}}

// No user-written constructors; implicit constructors are available.
struct B {
  var i : Int, j : Float
}
extension B {
  init(x : Int) {
    self.i = x
    self.j = 1.5
  }
}
B()
B(1)
B(1, 2.5)


struct F {
  var d : D
  var b : B
  var c : C
}

struct C {
  var d : D
  init(d : D) { } // suppress implicit constructors
}

struct D {
  var i : Int
  init(i : Int) { }
}

extension D {
  init() { i = 17 }
}

F() // expected-error{{expression does not type-check}}
D() // okay
B() // okay
C() // expected-error{{expression does not type-check}}

struct E {
  init(x : Wonka) { } // expected-error{{use of undeclared type 'Wonka'}}
}

var e : E

//===---
//===--- Tests for crashes.
//===---

//===--- rdar://14082378

struct NoCrash1a {
  init(NoCrash1b) {} // expected-error {{type annotation missing in pattern}}
}
var noCrash1c : NoCrash1a

