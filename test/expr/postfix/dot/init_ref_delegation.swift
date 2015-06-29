// RUN: %target-parse-verify-swift

// Tests for initializer delegation via self.init(...).

// Initializer delegation: classes
class C0 {
  convenience init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
}

class C1 {
  convenience init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Initializer delegation: structs
struct S0 {
  init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
}

struct S1 {
  init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Initializer delegation: enum
enum E0 {
  case A
  case B

  init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
}

enum E1 {
  case A
  case B

  init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Ill-formed initializer delegation: no matching constructor
class Z0 {
  init() { // expected-error {{designated initializer for 'Z0' cannot delegate (with 'self.init'); did you mean this to be a convenience initializer?}}
    self.init(5, 5) // expected-error{{cannot invoke 'init' with an argument list of type '(integer, integer)'}} expected-note{{delegation occurs here}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

struct Z1 {
  init() {
    self.init(5, 5) // expected-error{{cannot invoke 'init' with an argument list of type '(integer, integer)'}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

enum Z2 {
  case A
  case B

  init() {
    self.init(5, 5) // expected-error{{cannot invoke 'init' with an argument list of type '(integer, integer)'}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Ill-formed initialization: wrong context.
class Z3 {
  func f() {
    self.init() // expected-error{{'init' is a member of the type}}
  }

  init() { }
}

// 'init' is a static-ish member.
class Z4 {
  init() {} // expected-note{{selected non-required initializer}}

  convenience init(other: Z4) {
    other.init() // expected-error{{'init' is a member of the type}}
    other.dynamicType.init() // expected-error{{must use a 'required' initializer}} expected-warning{{unused}}
  }
}

class Z5 : Z4 {
  override init() { }

  convenience init(other: Z5) {
    other.init() // expected-error{{'init' is a member of the type}}
  }
}

// Ill-formed initialization: failure to call initializer.
class Z6 {
  convenience init() {
    var _ : () -> Z6 = self.init // expected-error{{partial application of 'self.init' initializer delegation is not allowed}}
  }

  init(other: Z6) { }
}

// Ill-formed initialization: both superclass and delegating.
class Z7Base { }

class Z7 : Z7Base {
  override init() { }

  init(b: Bool) {
    if b { super.init() } // expected-note{{previous chaining call is here}}
    else { self.init() } // expected-error{{initializer cannot both delegate ('self.init') and chain to a }}
  }
}

struct RDar16603812 {
   var i = 42
   init() {}
   func foo() {
      self.init() // expected-error {{'init' is a member of the type}}
      self.dynamicType.init() // expected-warning{{result of initializer is unused}}
   }
}

class RDar16666631 {
   var i: Int
   var d: Double
   var s: String
   init(i: Int, d: Double, s: String) {
      self.i = i
      self.d = d
      self.s = s
   }
   convenience init(i: Int, s: String) {
      self.init(i: i, d: 0.1, s: s)
   }
}
let rdar16666631 = RDar16666631(i: 5, d: 6) // expected-error {{missing argument for parameter 's' in call}}


struct S {
  init() {
    let x = S.init() // expected-warning{{}}
    self.init()
    let _ = self.init // expected-error{{}}

    let z = self.init() // expected-warning{{}} expected-note{{}} expected-warning{{}}
  }
}

class C {
  convenience init() { // expected-note 11 {{}}
    self.init()
    let x = self.init() // expected-warning{{}} expected-note{{}}
    let y: C = self.init() // expected-error{{}}
    let _: () -> C = self.init // expected-error{{}}
  }

  init(x: Int) {} // expected-note 11 {{}}

  required init(required: Double) {}
}

class D: C {
  override init(x: Int) {
    super.init(x: x)
    let x = super.init() // expected-warning{{}} expected-note{{}}
    let y: C = super.init() // expected-error{{}}
    let _: () -> C = super.init // expected-error{{}}
  }

  func foo() {
    self.init(x: 0) // expected-error{{}}
  }
  func bar() {
    super.init(x: 0) // expected-error{{}}
  }

  class func zim() -> Self {
    return self.init(required: 0)
  }

  class func zang() -> C {
    return super.init(required: 0)
  }

  required init(required: Double) {}
}

func init_tests() {
  var s = S.self
  var s1 = s.init()

  var ss1 = S.init()

  var c: C.Type = D.self
  var c1 = c.init(required: 0)
  var c2 = c.init(x: 0) // expected-error{{'required' initializer}}
  var c3 = c.init() // expected-error{{'required' initializer}}

  var c1a = c.init(required: 0)
  var c2a = c.init(x: 0) // expected-error{{'required' initializer}}
  var c3a = c.init() // expected-error{{'required' initializer}}

  var cf1: (Double) -> C = c.init
  var cf2: (Int) -> C    = c.init // expected-error{{'required' initializer}}
  var cf3: () -> C       = c.init // expected-error{{'required' initializer}}

  var cs1 = C.init(required: 0)
  var cs2 = C.init(x: 0)
  var cs3 = C.init()

  var csf1: (Double) -> C = C.init
  var csf2: (Int) -> C    = C.init
  var csf3: () -> C       = C.init

  var cs1a = C(required: 0)
  var cs2a = C(x: 0)
  var cs3a = C()

  var y = x.init() // expected-error{{}}
}

protocol P {
  init(proto: String)
}

func foo<T: C where T: P>(x: T, y: T.Type) {
  var c1 = x.dynamicType.init(required: 0)
  var c2 = x.dynamicType.init(x: 0) // expected-error{{'required' initializer}}
  var c3 = x.dynamicType.init() // expected-error{{'required' initializer}}
  var c4 = x.dynamicType.init(proto: "")

  var cf1: (Double) -> T = x.dynamicType.init
  var cf2: (Int) -> T    = x.dynamicType.init // expected-error{{'required' initializer}}
  var cf3: () -> T       = x.dynamicType.init // expected-error{{'required' initializer}}
  var cf4: (String) -> T = x.dynamicType.init

  var c1a = x.dynamicType.init(required: 0)
  var c2a = x.dynamicType.init(x: 0) // expected-error{{'required' initializer}}
  var c3a = x.dynamicType.init() // expected-error{{'required' initializer}}
  var c4a = x.dynamicType.init(proto: "")

  var ci1 = x.init(required: 0) // expected-error{{}}
  var ci2 = x.init(x: 0) // expected-error{{}}
  var ci3 = x.init() // expected-error{{}}
  var ci4 = x.init(proto: "") // expected-error{{}}

  var ci1a = x(required: 0) // expected-error{{}}
  var ci2a = x(x: 0) // expected-error{{}}
  var ci3a = x() // expected-error{{}}
  var ci4a = x(proto: "") // expected-error{{}}

  var cm1 = y.init(required: 0)
  var cm2 = y.init(x: 0) // expected-error{{'required' initializer}}
  var cm3 = y.init() // expected-error{{'required' initializer}}
  var cm4 = y.init(proto: "")

  var cm1a = y.init(required: 0)
  var cm2a = y.init(x: 0) // expected-error{{'required' initializer}}
  var cm3a = y.init() // expected-error{{'required' initializer}}
  var cm4a = y.init(proto: "")

  var cs1 = T.init(required: 0)
  var cs2 = T.init(x: 0) // expected-error{{'required' initializer}}
  var cs3 = T.init() // expected-error{{'required' initializer}}
  var cs4 = T.init(proto: "")
  var cs5 = T.init(notfound: "") // expected-error{{incorrect argument label}}

  var csf1: Double -> T = T.init
  var csf2: Int -> T    = T.init // expected-error{{'required' initializer}}
  var csf3: () -> T     = T.init // expected-error{{'required' initializer}}
  var csf4: String -> T = T.init

  var cs1a = T(required: 0)
  var cs2a = T(x: 0) // expected-error{{'required' initializer}}
  var cs3a = T() // expected-error{{'required' initializer}}
  var cs4a = T(proto: "")


}

