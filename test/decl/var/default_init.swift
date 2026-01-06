// RUN: %target-swift-frontend -typecheck -parse-as-library %s -verify -swift-version 5 -target %target-swift-5.1-abi-triple

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

class NotInitializableOptionalClass { // expected-error{{class 'NotInitializableOptionalClass' has no initializers}}
  // Do not perform default initialization for properties with explicitly-spelled 'Optional'.
  var opt: Optional<Int> // expected-note{{stored property 'opt' without initial value prevents synthesized initializers}}
}

struct NotInitializableOptionalStruct { // expected-note {{'init(opt:)' declared here}}
  var opt: Optional<Int>
}

func testBadDefaultInit() {
  _ = NotInitializableOptionalStruct() // expected-error {{missing argument for parameter 'opt' in call}}
  _ = NotInitializableOptionalClass() // expected-error {{'NotInitializableOptionalClass' cannot be constructed because it has no accessible initializers}}
}

// expected-error@+1{{actor 'NotInitializableActor' has no initializers}}
actor NotInitializableActor {

  // expected-note@+1{{stored property 'a' without initial value prevents synthesized initializers}}
  var a: Int
  // expected-note@+1{{stored property 'b' without initial value prevents synthesized initializers}}
  var b: Float
}
