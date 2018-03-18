// RUN: %target-typecheck-verify-swift

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
  init() { // expected-error {{designated initializer for 'Z0' cannot delegate (with 'self.init'); did you mean this to be a convenience initializer?}} {{3-3=convenience }}
    // expected-note @+2 {{delegation occurs here}}

    self.init(5, 5) // expected-error{{cannot invoke 'Z0.init' with an argument list of type '(Int, Int)'}}
    // expected-note @-1 {{overloads for 'Z0.init' exist with these partially matching parameter lists: (), (value: Int), (value: Double)}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

struct Z1 {
  init() {
    self.init(5, 5) // expected-error{{cannot invoke 'Z1.init' with an argument list of type '(Int, Int)'}}
  // expected-note @-1 {{overloads for 'Z1.init' exist with these partially matching parameter lists: (), (value: Int), (value: Double)}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

enum Z2 {
  case A
  case B

  init() {
    self.init(5, 5) // expected-error{{cannot invoke 'Z2.init' with an argument list of type '(Int, Int)'}}
    // expected-note @-1 {{overloads for 'Z2.init' exist with these partially matching parameter lists: (), (value: Int), (value: Double)}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Ill-formed initialization: wrong context.
class Z3 {
  func f() {
    self.init() // expected-error{{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}} {{10-10=type(of: }} {{14-14=)}} 
  }

  init() { }
}

// 'init' is a static-ish member.
class Z4 {
  init() {} // expected-note{{selected non-required initializer}}

  convenience init(other: Z4) {
    other.init() // expected-error{{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}} {{11-11=type(of: }} {{15-15=)}} 
    type(of: other).init() // expected-error{{must use a 'required' initializer}} expected-warning{{unused}}
  }
}

class Z5 : Z4 {
  override init() { }

  convenience init(other: Z5) {
    other.init() // expected-error{{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}} {{11-11=type(of: }} {{15-15=)}} 
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
      self.init() // expected-error {{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}} {{12-12=type(of: }} {{16-16=)}} 
      type(of: self).init() // expected-warning{{result of 'RDar16603812' initializer is unused}}
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
let rdar16666631 = RDar16666631(i: 5, d: 6) // expected-error {{incorrect argument label in call (have 'i:d:', expected 'i:s:')}}


struct S {
  init() {
    let _ = S.init()
    self.init()
    let _ = self.init // expected-error{{partial application of 'self.init' initializer delegation is not allowed}}
  }
}

class C {
  convenience init() { // expected-note 11 {{selected non-required initializer 'init()'}}
    self.init()
    let _: C = self.init() // expected-error{{cannot convert value of type '()' to specified type 'C'}}
    let _: () -> C = self.init // expected-error{{partial application of 'self.init' initializer delegation is not allowed}}
  }

  init(x: Int) {} // expected-note 11 {{selected non-required initializer 'init(x:)'}}

  required init(required: Double) {}
}

class D: C {
  override init(x: Int) {
    super.init(x: x)
    let _: C = super.init() // expected-error{{cannot convert value of type '()' to specified type 'C'}}
    let _: () -> C = super.init // expected-error{{partial application of 'super.init' initializer chain is not allowed}}
  }

  func foo() {
    self.init(x: 0) // expected-error{{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}} {{10-10=type(of: }} {{14-14=)}} 
  }
  func bar() {
    super.init(x: 0) // expected-error{{'super.init' cannot be called outside of an initializer}}
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

  var y = x.init() // expected-error{{use of unresolved identifier 'x'}}
}

protocol P {
  init(proto: String)
}

func foo<T: C>(_ x: T, y: T.Type) where T: P {
  var c1 = type(of: x).init(required: 0)
  var c2 = type(of: x).init(x: 0) // expected-error{{'required' initializer}}
  var c3 = type(of: x).init() // expected-error{{'required' initializer}}
  var c4 = type(of: x).init(proto: "")

  var cf1: (Double) -> T = type(of: x).init
  var cf2: (Int) -> T    = type(of: x).init // expected-error{{'required' initializer}}
  var cf3: () -> T       = type(of: x).init // expected-error{{'required' initializer}}
  var cf4: (String) -> T = type(of: x).init

  var c1a = type(of: x).init(required: 0)
  var c2a = type(of: x).init(x: 0) // expected-error{{'required' initializer}}
  var c3a = type(of: x).init() // expected-error{{'required' initializer}}
  var c4a = type(of: x).init(proto: "")

  var ci1 = x.init(required: 0) // expected-error{{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}} {{15-15=type(of: }} {{19-19=)}} 
  var ci2 = x.init(x: 0) // expected-error{{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}} {{15-15=type(of: }} {{19-19=)}} 
  var ci3 = x.init() // expected-error{{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}} {{15-15=type(of: }} {{19-19=)}} 
  var ci4 = x.init(proto: "") // expected-error{{'init' is a member of the type; use 'type(of: ...)' to initialize a new object of the same dynamic type}} {{15-15=type(of: }} {{19-19=)}} 

  var ci1a = x(required: 0) // expected-error{{cannot call value of non-function type 'T'}}
  var ci2a = x(x: 0) // expected-error{{cannot call value of non-function type 'T'}}
  var ci3a = x() // expected-error{{cannot call value of non-function type 'T'}}{{15-17=}}
  var ci4a = x(proto: "") // expected-error{{cannot call value of non-function type 'T'}}

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
  var cs5 = T.init(notfound: "") // expected-error{{argument labels '(notfound:)' do not match any available overloads}}
  // expected-note @-1 {{overloads for 'T.Type.init' exist with these partially matching parameter lists: (x: Int), (required: Double), (proto: String)}}

  var csf1: (Double) -> T = T.init
  var csf2: (Int) -> T    = T.init // expected-error{{'required' initializer}}
  var csf3: () -> T     = T.init // expected-error{{'required' initializer}}
  var csf4: (String) -> T = T.init

  var cs1a = T(required: 0)
  var cs2a = T(x: 0) // expected-error{{'required' initializer}}
  var cs3a = T() // expected-error{{'required' initializer}}
  var cs4a = T(proto: "")


}



class TestOverloadSets {
  convenience init() {
    self.init(5, 5) // expected-error{{cannot invoke 'TestOverloadSets.init' with an argument list of type '(Int, Int)'}}
    // expected-note @-1 {{overloads for 'TestOverloadSets.init' exist with these partially matching parameter lists: (), (a: Z0), (value: Int), (value: Double)}}
  }
  
  convenience init(a : Z0) {
    self.init(42 as Int8) // expected-error{{argument labels '(_:)' do not match any available overloads}}
    // expected-note @-1 {{overloads for 'TestOverloadSets.init' exist with these partially matching parameter lists: (a: Z0), (value: Int), (value: Double)}}
  }
  
  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

class TestNestedExpr {
  init() {}
  init?(fail: Bool) {}
  init(error: Bool) throws {}

  convenience init(a: Int) {
    let x: () = self.init() // expected-error {{initializer delegation ('self.init') cannot be nested in another statement}}
    // expected-warning@-1 {{immutable value 'x' was never used; consider replacing with '_' or removing it}}
  }

  convenience init(b: Int) {
    func use(_ x: ()) {}
    use(self.init()) // expected-error {{initializer delegation ('self.init') cannot be nested in another expression}}
  }

  convenience init(c: Int) {
    _ = ((), self.init()) // expected-error {{initializer delegation ('self.init') cannot be nested in another expression}}
  }

  convenience init(d: Int) {
    let x: () = self.init(fail: true)! // expected-error {{initializer delegation ('self.init') cannot be nested in another statement}}
    // expected-warning@-1 {{immutable value 'x' was never used; consider replacing with '_' or removing it}}
  }

  convenience init(e: Int) {
    func use(_ x: ()) {}
    use(self.init(fail: true)!) // expected-error {{initializer delegation ('self.init') cannot be nested in another expression}}
  }

  convenience init(f: Int) {
    _ = ((), self.init(fail: true)!) // expected-error {{initializer delegation ('self.init') cannot be nested in another expression}}
  }

  convenience init(g: Int) {
    let x: () = try! self.init(error: true) // expected-error {{initializer delegation ('self.init') cannot be nested in another statement}}
    // expected-warning@-1 {{immutable value 'x' was never used; consider replacing with '_' or removing it}}
  }

  convenience init(h: Int) {
    func use(_ x: ()) {}
    use(try! self.init(error: true)) // expected-error {{initializer delegation ('self.init') cannot be nested in another expression}}
  }

  convenience init(i: Int) {
    _ = ((), try! self.init(error: true)) // expected-error {{initializer delegation ('self.init') cannot be nested in another expression}}
  }

  convenience init(j: Int) throws {
    _ = {
      try self.init(error: true)
      // expected-error@-1 {{initializer delegation ('self.init') cannot be nested in another expression}}
    }

    _ = {
      do {
        try self.init(error: true)
        // expected-error@-1 {{initializer delegation ('self.init') cannot be nested in another expression}}
      }
    }

    defer {
      try! self.init(error: true)
      // expected-error@-1 {{initializer delegation ('self.init') cannot be nested in another expression}}
    }

    func local() throws {
      try self.init(error: true)
      // expected-error@-1 {{initializer delegation ('self.init') cannot be nested in another expression}}
    }
  }
}

class TestNestedExprSub : TestNestedExpr {
  init(a: Int) {
    let x: () = super.init() // expected-error {{initializer chaining ('super.init') cannot be nested in another statement}}
    // expected-warning@-1 {{immutable value 'x' was never used; consider replacing with '_' or removing it}}
  }

  init(b: Int) {
    func use(_ x: ()) {}
    use(super.init()) // expected-error {{initializer chaining ('super.init') cannot be nested in another expression}}
  }

  init(c: Int) {
    _ = ((), super.init()) // expected-error {{initializer chaining ('super.init') cannot be nested in another expression}}
  }

  init(d: Int) {
    let x: () = super.init(fail: true)! // expected-error {{initializer chaining ('super.init') cannot be nested in another statement}}
    // expected-warning@-1 {{immutable value 'x' was never used; consider replacing with '_' or removing it}}
  }

  init(e: Int) {
    func use(_ x: ()) {}
    use(super.init(fail: true)!) // expected-error {{initializer chaining ('super.init') cannot be nested in another expression}}
  }

  init(f: Int) {
    _ = ((), super.init(fail: true)!) // expected-error {{initializer chaining ('super.init') cannot be nested in another expression}}
  }

  init(g: Int) {
    let x: () = try! super.init(error: true) // expected-error {{initializer chaining ('super.init') cannot be nested in another statement}}
    // expected-warning@-1 {{immutable value 'x' was never used; consider replacing with '_' or removing it}}
  }

  init(h: Int) {
    func use(_ x: ()) {}
    use(try! super.init(error: true)) // expected-error {{initializer chaining ('super.init') cannot be nested in another expression}}
  }

  init(i: Int) {
    _ = ((), try! super.init(error: true)) // expected-error {{initializer chaining ('super.init') cannot be nested in another expression}}
  }
}


class TestOptionalTry {
  init() throws {}
  convenience init(a: Int) { // expected-note {{propagate the failure with 'init?'}} {{19-19=?}}
    try? self.init() // expected-error {{a non-failable initializer cannot use 'try?' to delegate to another initializer}}
    // expected-note@-1 {{force potentially-failing result with 'try!'}} {{5-9=try!}}
  }

  init?(fail: Bool) throws {}

  convenience init(failA: Int) { // expected-note {{propagate the failure with 'init?'}} {{19-19=?}}
    try? self.init(fail: true)! // expected-error {{a non-failable initializer cannot use 'try?' to delegate to another initializer}}
    // expected-note@-1 {{force potentially-failing result with 'try!'}} {{5-9=try!}}
  }

  convenience init(failB: Int) { // expected-note {{propagate the failure with 'init?'}} {{19-19=?}}
    try! self.init(fail: true) // expected-error {{a non-failable initializer cannot delegate to failable initializer 'init(fail:)' written with 'init?'}}
    // expected-note@-1 {{force potentially-failing result with '!'}} {{31-31=!}}
  }

  convenience init(failC: Int) {
    try! self.init(fail: true)! // okay
  }

  convenience init?(failD: Int) {
    try? self.init(fail: true) // okay
  }

  convenience init?(failE: Int) {
    try! self.init(fail: true) // okay
  }

  convenience init?(failF: Int) {
    try! self.init(fail: true)! // okay
  }

  convenience init?(failG: Int) {
    try? self.init(fail: true) // okay
  }
}

class TestOptionalTrySub : TestOptionalTry {
  init(a: Int) { // expected-note {{propagate the failure with 'init?'}} {{7-7=?}}
    try? super.init() // expected-error {{a non-failable initializer cannot use 'try?' to chain to another initializer}}
    // expected-note@-1 {{force potentially-failing result with 'try!'}} {{5-9=try!}}
  }
}
