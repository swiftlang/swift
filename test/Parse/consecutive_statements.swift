// RUN: %target-typecheck-verify-swift

func statement_starts() {
  var f : (Int) -> ()
  f = { (x : Int) -> () in }

  f(0)
  f (0)
  f // expected-warning{{variable is unused}}
  (0) // expected-warning {{integer literal is unused}}

  var a = [1,2,3]
  a[0] = 1
  a [0] = 1
  a // expected-warning{{variable is unused}}
  [0, 1, 2] // expected-warning {{expression of type '[Int]' is unused}}
}

// Within a function
func test(i: inout Int, j: inout Int) {
  // Okay
  let q : Int; i = j; j = i; _ = q

  if i != j { i = j }

  // Errors
  i = j j = i // expected-cparser-error{{consecutive statements}} {{8-8=;}} expected-astgen-error {{consecutive statements}}{{8-9= \n  }}
  let r : Int i = j // expected-cparser-error{{consecutive statements}} {{14-14=;}} expected-astgen-error {{expected '=' in variable}} expected-astgen-error {{cannot convert value of type '()' to specified type 'Int'}}
  let s : Int let t : Int // expected-cparser-error{{consecutive statements}} {{14-14=;}} expected-astgen-error {{consecutive statements}}{{14-15= \n  }}
  _ = r; _ = s; _ = t
}

struct X {
  // In a sequence of declarations.
  var a, b : Int func d() -> Int {} // expected-cparser-error{{consecutive declarations}} {{17-17=;}} expected-astgen-error {{consecutive declarations}}{{17-18= \n  }}

  var prop : Int { return 4
  } var other : Float // expected-cparser-error{{consecutive declarations}} {{4-4=;}} expected-astgen-error {{consecutive declarations}}{{4-5= \n  }}

  // Within property accessors
  subscript(i: Int) -> Float {
    get {
      var x = i x = i + x return Float(x) // expected-cparser-error{{consecutive statements}} {{16-16=;}} expected-astgen-error {{consecutive statements}}{{16-17= \n      }} expected-cparser-error{{consecutive statements}} {{26-26=;}} expected-astgen-error {{consecutive statements}}{{26-27= \n      }}
    }
    set {
      var x = i x = i + 1 // expected-cparser-error{{consecutive statements}} {{16-16=;}} expected-astgen-error {{consecutive statements}}{{16-17= \n      }}
      _ = x
    }
  }
}

class C {
  // In a sequence of declarations.
  var a, b : Int func d() -> Int {} // expected-cparser-error{{consecutive declarations}} {{17-17=;}} expected-astgen-error {{consecutive declarations}}{{17-18= \n  }}
  init() {
    a = 0
    b = 0
  }
}

protocol P {
  func a() func b() // expected-cparser-error{{consecutive declarations}} {{11-11=;}} expected-astgen-error {{consecutive declarations}}{{11-12= \n  }}
}

enum Color {
  case Red case Blue // expected-cparser-error{{consecutive declarations}} {{11-11=;}} expected-astgen-error {{consecutive declarations}}{{11-12= \n  }}
  func a() {} func b() {} // expected-cparser-error{{consecutive declarations}} {{14-14=;}} expected-astgen-error {{consecutive declarations}}{{14-15= \n  }}
}

// At the top level
var i, j : Int i = j j = i // expected-cparser-error{{consecutive statements}} {{15-15=;}} expected-astgen-error {{type annotation missing in pattern}} expected-cparser-error{{consecutive statements}} {{21-21=;}} expected-astgen-error {{expected '=' in variable}} expected-astgen-note {{'j' declared here}} expected-astgen-error {{cannot convert value of type '()' to specified type 'Int'}} expected-astgen-error {{use of global variable 'j' before its declaration}}

