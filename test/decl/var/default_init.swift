// RUN: %target-swift-frontend -typecheck -parse-as-library %s -verify

// Default initialization of variables.

class X { }

struct CanDefaultInit {
  var opt1: Int?
  var (opt2, (opt3, opt4)): (Int?, (Float?, Double?))
  weak var opt5: X?
}

func testCanDefaultInit() {
  _ = CanDefaultInit()
}


// Cases where we cannot perform default initialization.
class NotInitializable1 { // expected-error{{class 'NotInitializable1' has no initializers}}
  var (opt1, int1) : (Int?, Int) // expected-note{{stored properties 'opt1' and 'int1' without initial values prevent synthesized initializers}} {{33-33= = (nil, 0)}}
  let opt2: Int? // expected-note{{stored property 'opt2' without initial value prevents synthesized initializers}}
  var opt3: Int?
}

func localDefaultInit() -> Int? {
  let i: Int?
  return i
}


// <rdar://problem/16906000> Implicitly unwrapped optional let is not considered initialized, but var is
class DefaultInitOfLetProperty {
   let property: DefaultInitOfLetProperty!
   init(x: DefaultInitOfLetProperty) {
      self.property = DefaultInitOfLetProperty(x: self)
   }
}

var global: Int?

