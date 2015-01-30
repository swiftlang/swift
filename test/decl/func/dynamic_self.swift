// RUN: %target-parse-verify-swift

// ----------------------------------------------------------------------------
// DynamicSelf is only allowed on the return type of class and
// protocol methods.
func global() -> Self { } // expected-error{{global function cannot return 'Self'}}

func inFunction() {
  func local() -> Self { } // expected-error{{local function cannot return 'Self'}}
}

struct S0 {
  func f() -> Self { } // expected-error{{struct method cannot return 'Self'; did you mean to use the struct type 'S0'?}}{{15-19=S0}}

  func g(ds: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'S0'?}}{{14-18=S0}}
}

enum E0 {
  func f() -> Self { } // expected-error{{enum method cannot return 'Self'; did you mean to use the enum type 'E0'?}}{{15-19=E0}}

  func g(ds: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'E0'?}}{{14-18=E0}}
}

class C0 {
  func f() -> Self { } // okay

  func g(ds: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'C0'?}}{{14-18=C0}}

  func h(ds: Self) -> Self { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'C0'?}}{{14-18=C0}}
}

protocol P0 {
  func f() -> Self // okay

  func g(ds: Self) // okay
}

// ----------------------------------------------------------------------------
// The 'self' type of a Self method is based on Self
class C1 {
  required init(int i: Int) {}

  // Instance methods have a self of type Self.
  func f(b: Bool) -> Self {
    // FIXME: below diagnostic should complain about C1 -> Self conversion
    if b { return C1(int: 5) } // expected-error{{'C1' is not convertible to 'Self'}}

    // One can use .dynamicType to attempt to construct an object of type Self.
    if !b { return self.dynamicType(int: 5) }

    // Can't utter Self within the body of a method.
    var s2: Self = self // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'C1'?}}

    // Okay to return 'self', because it has the appropriate type.
    return self // okay
  }

  // Type methods have a self of type Self.Type.
  class func factory(b: Bool) -> Self {
    // Check directly.
    var x: Int = self // expected-error{{'Self.Type' is not convertible to 'Int'}}

    // Can't utter Self within the body of a method.
    var c1 = C1(int: 5) as Self // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'C1'?}}

    if b { return self(int: 5) }

    return Self() // expected-error{{use of unresolved identifier 'Self'}}
  }
}

// ----------------------------------------------------------------------------
// Using a method with a Self result carries the receiver type.
class X {
  func instance() -> Self {
  }

  class func factory() -> Self {
  }

  func produceX() -> X { }
}

class Y : X { 
  func produceY() -> Y { }
}

func testInvokeInstanceMethodSelf() {
  // Trivial case: invoking on the declaring class.
  var x = X()
  var x2 = x.instance()
  x = x2 // at least an X
  x2 = x // no more than an X

  // Invoking on a subclass.
  var y = Y()
  var y2 = y.instance();
  y = y2 // at least a Y
  y2 = y // no more than a Y
}

func testInvokeTypeMethodSelf() {
  // Trivial case: invoking on the declaring class.
  var x = X()
  var x2 = X.factory()
  x = x2 // at least an X
  x2 = x // no more than an X

  // Invoking on a subclass.
  var y = Y()
  var y2 = Y.factory()
  y = y2 // at least a Y
  y2 = y // no more than a Y
}

func testCurryInstanceMethodSelf() {
  // Trivial case: currying on the declaring class.
  var produceX = X.produceX
  var produceX2 = X.instance
  produceX = produceX2
  produceX2 = produceX

  // Currying on a subclass.
  var produceY = Y.produceY
  var produceY2 = Y.instance
  produceY = produceY2
  produceY2 = produceY
}

class GX<T> {
  func instance() -> Self {
  }

  class func factory() -> Self {
  }

  func produceGX() -> GX { }
}

class GY<T> : GX<[T]> { 
  func produceGY() -> GY { }
}

func testInvokeInstanceMethodSelfGeneric() {
  // Trivial case: invoking on the declaring class.
  var x = GX<Int>()
  var x2 = x.instance()
  x = x2 // at least an GX<Int>
  x2 = x // no more than an GX<Int>

  // Invoking on a subclass.
  var y = GY<Int>()
  var y2 = y.instance();
  y = y2 // at least a GY<Int>
  y2 = y // no more than a GY<Int>
}

func testInvokeTypeMethodSelfGeneric() {
  // Trivial case: invoking on the declaring class.
  var x = GX<Int>()
  var x2 = GX<Int>.factory()
  x = x2 // at least an GX<Int>
  x2 = x // no more than an GX<Int>

  // Invoking on a subclass.
  var y = GY<Int>()
  var y2 = GY<Int>.factory();
  y = y2 // at least a GY<Int>
  y2 = y // no more than a GY<Int>
}

func testCurryInstanceMethodSelfGeneric() {
  // Trivial case: currying on the declaring class.
  var produceGX = GX<Int>.produceGX
  var produceGX2 = GX<Int>.instance
  produceGX = produceGX2
  produceGX2 = produceGX

  // Currying on a subclass.
  var produceGY = GY<Int>.produceGY
  var produceGY2 = GY<Int>.instance
  produceGY = produceGY2
  produceGY2 = produceGY
}

// ----------------------------------------------------------------------------
// Overriding a method with a Self
class Z : Y { 
  override func instance() -> Self {
  }

  override class func factory() -> Self {
  }
}

func testOverriddenMethodSelfGeneric() {
  var z = Z()

  var z2 = z.instance();
  z = z2
  z2 = z

  var z3 = Z.factory()
  z = z3
  z3 = z
}

// ----------------------------------------------------------------------------
// Generic uses of Self methods.
protocol P {
  func f() -> Self
}

func testGenericCall<T: P>(var t: T) {
  var t2 = t.f()
  t2 = t
  t = t2
}

// ----------------------------------------------------------------------------
// Existential uses of Self methods.
func testExistentialCall(p: P) {
  var p2 = p.f()
}

// ----------------------------------------------------------------------------
// Dynamic lookup of Self methods.
@objc class SomeClass {
  @objc func method() -> Self { return self }
}

func testAnyObject(var ao: AnyObject) {
  var result : AnyObject = ao.method!()
  result = ao
  ao = result
}

// ----------------------------------------------------------------------------
// Name lookup on Self values
extension Y {
  func testInstance() -> Self {
    if false { return self.instance() }
    return instance()
  }

  class func testFactory() -> Self {
    if false { return self.factory() }
    return factory()
  }
}

// ----------------------------------------------------------------------------
// Optional Self returns

extension X {
  func tryToClone() -> Self? { return nil }
  func cloneOrFail() -> Self { return self }
  func cloneAsObjectSlice() -> X? { return self }
}
extension Y {
  func operationThatOnlyExistsOnY() {}
}
func testOptionalSelf(y : Y) {
  if let clone = y.tryToClone() {
    clone.operationThatOnlyExistsOnY()
  }

  // Sanity-checking to make sure that the above succeeding
  // isn't coincidental.
  if let clone = y.cloneOrFail() { // expected-error {{bound value in a conditional binding must be of Optional type}}
    clone.operationThatOnlyExistsOnY()
  }

  // Sanity-checking to make sure that the above succeeding
  // isn't coincidental.
  if let clone = y.cloneAsObjectSlice() {
    clone.operationThatOnlyExistsOnY() // expected-error {{'X' does not have a member named 'operationThatOnlyExistsOnY'}}
  }
}
