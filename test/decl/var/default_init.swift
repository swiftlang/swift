// RUN: %swift -parse %s -verify 

// Default initialization of variables.

class X { }

struct CanDefaultInit {
  var opt1: Int?
  var (opt2, (opt3, opt4)): (Int?, (Float?, Double?))
  weak var opt5: X?
}

func testCanDefaultInit() {
// FIXME: rdar://16921173 This should be accepted.
  CanDefaultInit()  // expected-error {{missing argument for parameter 'opt1' in call}}
}


// Cases where we cannot perform default initialization.
class NotInitializable1 { // expected-error{{class 'NotInitializable1' has no initializers}}
  var (opt1, int1) : (Int?, Int) // expected-note{{stored properties 'opt1' and 'int1' without initial values prevent synthesized initializers}}
  let opt2: Int? // expected-note{{stored property 'opt2' without initial value prevents synthesized initializers}}
}

func localDefaultInit() -> Int? {
  var i: Int?
  return i
}
