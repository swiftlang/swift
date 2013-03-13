// RUN: %swift %s -verify

// Within a function
func test(i : Int, j : Int) {
   // Okay
   var x : Int; i = j; j = i
   if i != j { i = j } 

   // Errors
   i = j j = i // expected-error{{consecutive statements}}
   var y : Int i = j // expected-error{{consecutive statements}}
   var z : Int var z2 : Int // expected-error{{consecutive statements}}
}

struct X {
  // In a sequence of declarations.
  var a, b : Int func d() -> Int {} // expected-error{{consecutive declarations}}

  var prop : Int {
  } var other : Float // expected-error{{consecutive declarations}}

  // Within property accessors
  subscript (i : Int) -> Float {
  get:
    var x = i x = i + 1 return Float(x) // expected-error{{consecutive statements}} expected-error{{consecutive statements}}

  set:
    var x = i x = i + 1 // expected-error{{consecutive statements}}
  }
}

class C {
  // In a sequence of declarations.
  var a, b : Int func d() -> Int {} // expected-error{{consecutive declarations}}
}

protocol P {
  func a() func b() // expected-error{{consecutive declarations}}
}

oneof Color {
  Red, Blue
  func a() {} func b() {} // expected-error{{consecutive declarations}}
}

// At the top level
var i, j : Int i = j j = i // expected-error{{consecutive statements}} expected-error{{consecutive statements}}
