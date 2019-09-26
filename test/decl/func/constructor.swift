// RUN: %target-typecheck-verify-swift

// User-written default constructor
struct X {
  init() {}
}

X() // expected-warning{{unused}}

// User-written memberwise constructor
struct Y {
  var i : Int, f : Float
  init(i : Int, f : Float) {}
}

Y(i: 1, f: 1.5) // expected-warning{{unused}}

// User-written memberwise constructor with default
struct Z {
  var a : Int
  var b : Int

  init(a : Int, b : Int = 5) {
    self.a = a
    self.b = b
  }
}

Z(a: 1, b: 2) // expected-warning{{unused}}

// User-written init suppresses implicit constructors.
struct A {
  var i, j : Int
  
  init(x : Int) { // expected-note {{'init(x:)' declared here}}
    i = x
    j = x
  }
}

A() // expected-error{{missing argument for parameter 'x'}}
A(x: 1) // expected-warning{{unused}}
A(1, 1) // expected-error{{extra argument in call}}

// No user-written constructors; implicit constructors are available.
struct B {
  var i : Int = 0, j : Float = 0.0
}
extension B {
  init(x : Int) {
    self.i = x
    self.j = 1.5
  }
}
B() // expected-warning{{unused}}
B(x: 1) // expected-warning{{unused}}
B(i: 1, j: 2.5) // expected-warning{{unused}}


struct F {  // expected-note {{'init(d:b:c:)' declared here}}
  var d : D
  var b : B
  var c : C
}

struct C {
  var d : D

  // suppress implicit initializers
  init(d : D) { } // expected-note {{'init(d:)' declared here}}
}

struct D {
  var i : Int
  init(i : Int) { }
}

extension D {
  init() { i = 17 }
}

F() // expected-error{{missing arguments for parameters 'd', 'b', 'c' in call}}
D() // okay // expected-warning{{unused}}
B() // okay // expected-warning{{unused}}
C() // expected-error{{missing argument for parameter 'd'}}

struct E {
  init(x : Wonka) { } // expected-error{{use of undeclared type 'Wonka'}}
}

var e : E

//----------------------------------------------------------------------------
// Argument/parameter name separation
//----------------------------------------------------------------------------
class ArgParamSep {
  init(_ b: Int, _: Int, forInt int: Int, c _: Int, d: Int) { }
}

// Tests for crashes.
// rdar://14082378

struct NoCrash1a {
  init(_: NoCrash1b) {} // expected-error {{use of undeclared type 'NoCrash1b'}}
}
var noCrash1c : NoCrash1a

class MissingDef {
  init() // expected-error{{initializer requires a body}}
}
