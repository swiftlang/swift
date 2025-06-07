// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_feature_LifetimeDependence

// Coverage testing for LifetimeDependence inferrence logic. The tests are grouped according to the design of
// LifetimeDependenceChecker.

class C {}

struct NE: ~Escapable {}

struct NEImmortal: ~Escapable {
  @lifetime(immortal)
  init() {}
}

// =============================================================================
// Handle non-Escapable results with 'self'
// =============================================================================

struct NonEscapableSelf: ~Escapable {
  func methodNoParam() -> NonEscapableSelf { self } // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}

  @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
  func methodNoParamLifetime() -> NonEscapableSelf { self }

  @lifetime(copy self) // OK
  func methodNoParamCopy() -> NonEscapableSelf { self }

  @lifetime(borrow self) // OK
  func methodNoParamBorrow() -> NonEscapableSelf { self }

  mutating func mutatingMethodNoParam() -> NonEscapableSelf { self } // expected-error{{a mutating method with a ~Escapable result requires '@lifetime(...)'}}
 // expected-error@-1{{a mutating method with a ~Escapable 'self' requires '@lifetime(self: ...)'}}

  @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
  mutating func mutatingMethodNoParamLifetime() -> NonEscapableSelf { self }

  @lifetime(copy self) // OK
  mutating func mutatingMethodNoParamCopy() -> NonEscapableSelf { self }

  @lifetime(borrow self) // OK
  mutating func mutatingMethodNoParamBorrow() -> NonEscapableSelf { self }

  func methodOneParam(_: Int) -> NonEscapableSelf { self } // expected-error{{a method with a ~Escapable result requires '@lifetime(...)'}}

  @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
  func methodOneParamLifetime(_: Int) -> NonEscapableSelf { self }

  @lifetime(copy self) // OK
  func methodOneParamCopy(_: Int) -> NonEscapableSelf { self }

  @lifetime(borrow self) // OK
  func methodOneParamBorrow(_: Int) -> NonEscapableSelf { self }

  mutating func mutatingMethodOneParam(_: Int) -> NonEscapableSelf { self } // expected-error{{a mutating method with a ~Escapable result requires '@lifetime(...)'}}
  // expected-error@-1{{a mutating method with a ~Escapable 'self' requires '@lifetime(self: ...)'}}

  @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
  mutating func mutatingMethodOneParamLifetime(_: Int) -> NonEscapableSelf { self }

  @lifetime(copy self) // OK
  mutating func mutatingMethodOneParamCopy(_: Int) -> NonEscapableSelf { self }

  @lifetime(borrow self) // OK
  mutating func mutatingMethodOneParamBorrow(_: Int) -> NonEscapableSelf { self }
}

struct EscapableTrivialSelf {
  func methodNoParam() -> NEImmortal { NEImmortal() } // expected-error{{cannot infer lifetime dependence on a method because 'self' is BitwiseCopyable}}

  @lifetime(self) // OK
  func methodNoParamLifetime() -> NEImmortal { NEImmortal() }

  @lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type, use '@lifetime(borrow self)' instead}}
  func methodNoParamCopy() -> NEImmortal { NEImmortal() }

  @lifetime(borrow self) // OK
  func methodNoParamBorrow() -> NEImmortal { NEImmortal() }

  func mutatingMethodNoParam() -> NEImmortal { NEImmortal() } // expected-error{{cannot infer lifetime dependence on a method because 'self' is BitwiseCopyable}}

  @lifetime(self) // OK
  mutating func mutatingMethodNoParamLifetime() -> NEImmortal { NEImmortal() }

  @lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type, use '@lifetime(&self)' instead}}
  mutating func mutatingMethodNoParamCopy() -> NEImmortal { NEImmortal() }

  @lifetime(borrow self)
  mutating func mutatingMethodNoParamBorrow() -> NEImmortal { NEImmortal() }

  func methodOneParam(_: Int) -> NEImmortal { NEImmortal() } // expected-error{{a method with a ~Escapable result requires '@lifetime(...)'}}

  @lifetime(self)
  func methodOneParamLifetime(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type, use '@lifetime(borrow self)' instead}}
  func methodOneParamCopy(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(borrow self)
  func methodOneParamBorrow(_: Int) -> NEImmortal { NEImmortal() }

  mutating func mutatingMethodOneParam(_: Int) -> NEImmortal { NEImmortal() } // expected-error{{a mutating method with a ~Escapable result requires '@lifetime(...)'}}

  @lifetime(self)
  mutating func mutatingMethodOneParamLifetime(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type, use '@lifetime(&self)' instead}}
  mutating func mutatingMethodOneParamCopy(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(borrow self)
  mutating func mutatingMethodOneParamBorrow(_: Int) -> NEImmortal { NEImmortal() }
}

struct EscapableNonTrivialSelf {
  let c: C

  init(c: C) { self.c = c }

  func methodNoParam() -> NEImmortal { NEImmortal() }

  @lifetime(self)
  func methodNoParamLifetime() -> NEImmortal { NEImmortal() }

  @lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type, use '@lifetime(borrow self)' instead}}
  func methodNoParamCopy() -> NEImmortal { NEImmortal() }

  @lifetime(borrow self)
  func methodNoParamBorrow() -> NEImmortal { NEImmortal() }

  mutating func mutatingMethodNoParam() -> NEImmortal { NEImmortal() }

  func methodInoutNonEscapableParam(_: inout NE) {}

  mutating func mutatingMethodInoutNonEscapableParam(_: inout NE) {}

  @lifetime(self)
  mutating func mutatingMethodNoParamLifetime() -> NEImmortal { NEImmortal() }

  @lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type, use '@lifetime(&self)' instead}}
  mutating func mutatingMethodNoParamCopy() -> NEImmortal { NEImmortal() }

  @lifetime(&self)
  mutating func mutatingMethodNoParamBorrow() -> NEImmortal { NEImmortal() }

  func methodOneParam(_: Int) -> NEImmortal { NEImmortal() } // expected-error{{a method with a ~Escapable result requires '@lifetime(...)'}}

  @lifetime(self)
  func methodOneParamLifetime(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type, use '@lifetime(borrow self)' instead}}
  func methodOneParamCopy(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(borrow self)
  func methodOneParamBorrow(_: Int) -> NEImmortal { NEImmortal() }

  mutating func mutatingMethodOneParam(_: Int) -> NEImmortal { NEImmortal() } // expected-error{{a mutating method with a ~Escapable result requires '@lifetime(...)'}}

  @lifetime(self)
  mutating func mutatingMethodOneParamLifetime(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type, use '@lifetime(&self)' instead}}
  mutating func mutatingMethodOneParamCopy(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(&self)
  mutating func mutatingMethodOneParamBorrow(_: Int) -> NEImmortal { NEImmortal() }
}

// =============================================================================
// Handle non-Escapable results which must depend on a parameter
// (for initializers and stand-alone functions)
// =============================================================================

// An implicit initializer illegally consumes its nontrivial parameter.
public struct NonescapableImplicitInitializer: ~Escapable {
  // expected-error @-1{{cannot borrow the lifetime of 'c', which has consuming ownership on an implicit initializer}}
  var c: C
}

struct NonescapableInitializers: ~Escapable {
  var c: C

  init() { c = C() } // expected-error{{an initializer with a ~Escapable result needs a parameter to depend on}}
  // expected-note@-1{{'@lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
 
  init(c: C) { self.c = c } // expected-error{{cannot borrow the lifetime of 'c', which has consuming ownership on an initializer}}

  init(c: C, _: Int) { self.c = c } // expected-error{{an initializer with a ~Escapable result requires '@lifetime(...)'}}

  init(ne: NE) { c = C() } // expected-error{{cannot infer the lifetime dependence scope on an initializer with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}
}

struct NonescapableConsumingInitializers: ~Escapable {
  var c: C // implicit get/set is OK

  init(c: consuming C) { self.c = c } // expected-error{{cannot borrow the lifetime of 'c', which has consuming ownership on an initializer}}

  init(c: consuming C, _: Int) { self.c = c } // expected-error{{an initializer with a ~Escapable result requires '@lifetime(...)'}}

  init(ne: consuming NE) { c = C() } // expected-error{{cannot infer the lifetime dependence scope on an initializer with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}
}

struct NonescapableBorrowingInitializers: ~Escapable {
  var c: C // implicit stored property set is OK

  init(c: borrowing C) { self.c = c } // OK

  init(c: borrowing C, _: Int) { self.c = c } // expected-error{{an initializer with a ~Escapable result requires '@lifetime(...)'}}

  init(ne: borrowing NE) { c = C() } // expected-error{{cannot infer the lifetime dependence scope on an initializer with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}
}

struct NonescapableInoutInitializers: ~Escapable {
  var c: C // implicit stored property set is OK

  init(c: inout C) { self.c = c } // OK

  init(c: inout C, _: Int) { self.c = c } // expected-error{{an initializer with a ~Escapable result requires '@lifetime(...)'}}

  init(ne: inout NE) { c = C() } // expected-error{{cannot infer the lifetime dependence scope on an initializer with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}
}

func noParam() -> NEImmortal { NEImmortal() } // expected-error{{a function with a ~Escapable result needs a parameter to depend on}}
// expected-note@-1{{'@lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}

@lifetime(immortal)
func noParamImmortal() -> NEImmortal { NEImmortal() } // OK

func oneParam(c: C) -> NEImmortal { NEImmortal() }

@lifetime(c)
func oneParamLifetime(c: C) -> NEImmortal { NEImmortal() }

func oneParamConsume(c: consuming C) -> NEImmortal { NEImmortal() } // expected-error{{cannot borrow the lifetime of 'c', which has consuming ownership on a function}}

@lifetime(c) // expected-error{{invalid lifetime dependence on an Escapable value with consuming ownership}}
func oneParamConsumeLifetime(c: consuming C) -> NEImmortal { NEImmortal() }

func oneParamBorrow(c: borrowing C) -> NEImmortal { NEImmortal() } // OK

@lifetime(c)
func oneParamBorrowLifetime(c: borrowing C) -> NEImmortal { NEImmortal() } // OK

func oneParamInout(c: inout C) -> NEImmortal { NEImmortal() } // OK

@lifetime(c)
func oneParamInoutLifetime(c: inout C) -> NEImmortal { NEImmortal() } // OK

func twoParams(c: C, _: Int) -> NEImmortal { NEImmortal() } // expected-error{{a function with a ~Escapable result requires '@lifetime(...)'}}

@lifetime(c)
func twoParamsLifetime(c: C, _: Int) -> NEImmortal { NEImmortal() }

func twoParamsConsume(c: consuming C, _: Int) -> NEImmortal { NEImmortal() } // expected-error{{a function with a ~Escapable result requires '@lifetime(...)'}}

func twoParamsBorrow(c: borrowing C, _: Int) -> NEImmortal { NEImmortal() } // expected-error{{a function with a ~Escapable result requires '@lifetime(...)'}}

func twoParamsInout(c: inout C, _: Int) -> NEImmortal { NEImmortal() } // expected-error{{a function with a ~Escapable result requires '@lifetime(...)'}}

func neParam(ne: NE) -> NE { ne } // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}

@lifetime(ne) // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}
func neParamLifetime(ne: NE) -> NE { ne }

func neParamBorrow(ne: borrowing NE) -> NE { ne } // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}

@lifetime(ne) // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}
func neParamBorrowLifetime(ne: borrowing NE) -> NE { ne }

func neParamConsume(ne: consuming NE) -> NE { ne } // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}

@lifetime(ne) // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}
func neParamConsumeLifetime(ne: consuming NE) -> NE { ne }

func neParamInout(ne: inout NE) -> NE { ne } // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}

@lifetime(ne) // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@lifetime(borrow ne)' or '@lifetime(copy ne)'}}
func neParamInoutLifetime(ne: inout NE) -> NE { ne }

func neTwoParam(ne: NE, _:Int) -> NE { ne } // expected-error{{a function with a ~Escapable result requires '@lifetime(...)'}}

func voidInoutOneParam(_: inout NE) {} // OK

func voidInoutTwoParams(_: inout NE, _: Int) {} // OK

// =============================================================================
// Handle Accessors:
//
// 'get', '_read', and '_modify' are inferred as methods that return ~Escpable results dependent on 'self'
//
// 'set' is only inferred when implicit. This allows for the declaration of non-Escapable stored properties. Knowing
// that the implicit setter assigns a stored property is sufficient for the compiler to assume Inherit dependency on
// both 'self' and 'newValue'. A full assignment would not need the 'self' dependency.
// =============================================================================

struct Accessors {
  let c: C

  var neComputed: NEImmortal {
    get { // OK
      NEImmortal()
    }

    set { // OK (no dependency)
    }
  }

  var neYielded: NEImmortal {
    _read { // OK
      yield NEImmortal()
    }

    _modify { // OK
      var ne = NEImmortal()
      yield &ne
    }
  }
}

struct NonescapableSelfAccessors: ~Escapable {
  var ne: NE

  @lifetime(immortal)
  init() {
    ne = NE()
  }

  var neComputed: NE {
    get { // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
      ne
    }

    set { // expected-error{{a mutating method with a ~Escapable 'self' requires '@lifetime(self: ...)'}}
      ne = newValue
    }
  }

  var neYielded: NE {
    _read { // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
      yield ne
    }

    @lifetime(borrow self)
    _modify {
      yield &ne
    }
  }

  var neComputedLifetime: NE {
    @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
    get {
      ne
    }

    @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
    set {
      ne = newValue
    }
  }

  var neYieldedLifetime: NE {
    @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
    _read {
      yield ne
    }

    @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
    _modify {
      yield &ne
    }
  }

  var neComputedCopy: NE {
    @lifetime(copy self)
    get {
      ne
    }

    @lifetime(copy self)
    set {
      ne = newValue
    }
  }

  var neYieldedCopy: NE {
    @lifetime(copy self)
    _read {
      yield ne
    }

    @lifetime(copy self)
    _modify {
      yield &ne
    }
  }

  var neComputedBorrow: NE {
    @lifetime(borrow self)
    get {
      ne
    }

    @lifetime(borrow self)
    set {
      ne = newValue
    }
  }

  var neYieldedBorrow: NE {
    @lifetime(borrow self)
    _read {
      yield ne
    }

    @lifetime(borrow self)
    _modify {
      yield &ne
    }
  }
}

struct NoncopyableSelfAccessors: ~Copyable & ~Escapable {
  var ne: NE

  var neComputed: NE {
    get { // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
      ne
    }

    set { // expected-error{{a mutating method with a ~Escapable 'self' requires '@lifetime(self: ...)'}}
      ne = newValue
    }
  }

  var neYielded: NE {
    _read { // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
      yield ne
    }

    @lifetime(&self)
    _modify {
      yield &ne
    }
  }

  var neComputedLifetime: NE {
    @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
    get {
      ne
    }

    @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
    set {
      ne = newValue
    }
  }

  var neYieldedLifetime: NE {
    @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
    _read {
      yield ne
    }

    @lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
    _modify {
      yield &ne
    }
  }

  var neComputedCopy: NE {
    @lifetime(copy self)
    get {
      ne
    }

    @lifetime(copy self)
    set {
      ne = newValue
    }
  }

  var neYieldedCopy: NE {
    @lifetime(copy self)
    _read {
      yield ne
    }

    @lifetime(copy self)
    _modify {
      yield &ne
    }
  }

  var neComputedBorrow: NE {
    @lifetime(borrow self)
    get {
      ne
    }

    @lifetime(&self)
    set {
      ne = newValue
    }
  }

  var neYieldedBorrow: NE {
    @lifetime(borrow self)
    _read {
      yield ne
    }

    @lifetime(&self)
    _modify {
      yield &ne
    }
  }
}

// =============================================================================
// Handle mutating methods with no return value
// =============================================================================

struct NonEscapableMutableSelf: ~Escapable {
  // This is unambiguous: inout 'self' needs a dependency, and it can't be a borrow dependency because the original
  // value is consumed.
  /* @lifetime(self: copy self) */
  mutating func mutatingMethodNoParam() {} // OK

  @lifetime(self: self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
  mutating func mutatingMethodNoParamLifetime() {}

  @lifetime(self: copy self) // OK
  mutating func mutatingMethodNoParamCopy() {}

  @lifetime(self: borrow self) // expected-error{{invalid use of borrow dependence on the same inout parameter}}
  mutating func mutatingMethodNoParamBorrow() {}

  mutating func mutatingMethodOneParam(_: NE) {} // expected-error{{a mutating method with a ~Escapable 'self' requires '@lifetime(self: ...)'}}

  @lifetime(self: self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@lifetime(borrow self)' or '@lifetime(copy self)'}}
  mutating func mutatingMethodOneParamLifetime(_: NE) {}

  @lifetime(copy self) // OK
  mutating func mutatingMethodOneParamCopy(_: NE) {}

  @lifetime(borrow self)
  mutating func mutatingMethodOneParamBorrow(_: NE) {}
}
