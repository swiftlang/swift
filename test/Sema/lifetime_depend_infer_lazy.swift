// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-lifetime-dependence-inference

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
  func methodNoParam() -> NonEscapableSelf { self } // OK

  @lifetime(self) // OK
  func methodNoParamLifetime() -> NonEscapableSelf { self }

  @lifetime(copy self) // OK
  func methodNoParamCopy() -> NonEscapableSelf { self }

  @lifetime(borrow self) // OK
  func methodNoParamBorrow() -> NonEscapableSelf { self }

  @lifetime(self) // OK
  mutating func mutatingMethodNoParamLifetime() -> NonEscapableSelf { self }

  @lifetime(copy self) // OK
  mutating func mutatingMethodNoParamCopy() -> NonEscapableSelf { self }

  @lifetime(borrow self) // OK
  mutating func mutatingMethodNoParamBorrow() -> NonEscapableSelf { self }

  func methodOneParam(_: Int) -> NonEscapableSelf { self } // OK

  @lifetime(self) // OK
  func methodOneParamLifetime(_: Int) -> NonEscapableSelf { self }

  @lifetime(copy self) // OK
  func methodOneParamCopy(_: Int) -> NonEscapableSelf { self }

  @lifetime(borrow self) // OK
  func methodOneParamBorrow(_: Int) -> NonEscapableSelf { self }

  @lifetime(self) // OK
  mutating func mutatingMethodOneParamLifetime(_: Int) -> NonEscapableSelf { self }

  @lifetime(copy self) // OK
  mutating func mutatingMethodOneParamCopy(_: Int) -> NonEscapableSelf { self }

  @lifetime(borrow self) // OK
  mutating func mutatingMethodOneParamBorrow(_: Int) -> NonEscapableSelf { self }
}

struct EscapableTrivialSelf {
  @lifetime(self) // OK
  func methodNoParamLifetime() -> NEImmortal { NEImmortal() }

  @lifetime(borrow self) // OK
  func methodNoParamBorrow() -> NEImmortal { NEImmortal() }

  @lifetime(self) // OK
  mutating func mutatingMethodNoParamLifetime() -> NEImmortal { NEImmortal() }

  @lifetime(borrow self) // OK
  mutating func mutatingMethodNoParamBorrow() -> NEImmortal { NEImmortal() }

  @lifetime(self) // OK
  func methodOneParamLifetime(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(borrow self) // OK
  func methodOneParamBorrow(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(self) // OK
  mutating func mutatingMethodOneParamLifetime(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(borrow self)
  mutating func mutatingMethodOneParamBorrow(_: Int) -> NEImmortal { NEImmortal() }
}

struct EscapableNonTrivialSelf {
  let c: C

  init(c: C) { self.c = c }

  func methodNoParam() -> NEImmortal { NEImmortal() } // OK

  @lifetime(self) // OK
  func methodNoParamLifetime() -> NEImmortal { NEImmortal() }

  @lifetime(borrow self) // OK
  func methodNoParamBorrow() -> NEImmortal { NEImmortal() }

  func mutatingMethodNoParam() -> NEImmortal { NEImmortal() } // OK

  @lifetime(self)
  mutating func mutatingMethodNoParamLifetime() -> NEImmortal { NEImmortal() }

  @lifetime(&self)
  mutating func mutatingMethodNoParamBorrow() -> NEImmortal { NEImmortal() }

  func methodOneParam(_: Int) -> NEImmortal { NEImmortal() } // OK

  @lifetime(self) // OK
  func methodOneParamLifetime(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(borrow self) // OK
  func methodOneParamBorrow(_: Int) -> NEImmortal { NEImmortal() }

  mutating func mutatingMethodOneParam(_: Int) -> NEImmortal { NEImmortal() } // OK

  @lifetime(self) // OK
  mutating func mutatingMethodOneParamLifetime(_: Int) -> NEImmortal { NEImmortal() }

  @lifetime(&self) // OK
  mutating func mutatingMethodOneParamBorrow(_: Int) -> NEImmortal { NEImmortal() }
}

// =============================================================================
// Handle non-Escapable results which must depend on a parameter
// (for initializers and stand-alone functions)
// =============================================================================

struct NonescapableInitializers: ~Escapable {
  var c: C

  init(ne: NE) { c = C() } // OK
}

struct NonescapableConsumingInitializers: ~Escapable {
  var c: C // implicit get/set is OK

  init(ne: consuming NE) { c = C() } // OK
}

struct NonescapableBorrowingInitializers: ~Escapable {
  var c: C // implicit stored property set is OK

  init(c: borrowing C) { self.c = copy c } // OK

  init(c: borrowing C, _: Int) { self.c = copy c } // OK

  init(ne: borrowing NE) { c = C() } // OK
}

struct NonescapableInoutInitializers: ~Escapable {
  var c: C // implicit stored property set is OK

  init(c: inout C) { self.c = copy c } // OK
}

@lifetime(immortal)
func noParamImmortal() -> NEImmortal { NEImmortal() } // OK

@lifetime(c)
func oneParamLifetime(c: C) -> NEImmortal { NEImmortal() }

func oneParamBorrow(c: borrowing C) -> NEImmortal { NEImmortal() } // OK

@lifetime(c)
func oneParamBorrowLifetime(c: borrowing C) -> NEImmortal { NEImmortal() } // OK

func oneParamInout(c: inout C) -> NEImmortal { NEImmortal() } // OK

@lifetime(c)
func oneParamInoutLifetime(c: inout C) -> NEImmortal { NEImmortal() } // OK

@lifetime(c)
func twoParamsLifetime(c: C, _: Int) -> NEImmortal { NEImmortal() }

func twoParamsBorrow(c: borrowing C, _: Int) -> NEImmortal { NEImmortal() } // OK

func neParam(ne: NE) -> NE { ne } // OK

@lifetime(ne) // OK
func neParamLifetime(ne: NE) -> NE { ne }

func neParamBorrow(ne: borrowing NE) -> NE { copy ne } // OK

@lifetime(ne) // OK
func neParamBorrowLifetime(ne: borrowing NE) -> NE { copy ne }

func neParamConsume(ne: consuming NE) -> NE { ne } // OK

@lifetime(ne) // OK
func neParamConsumeLifetime(ne: consuming NE) -> NE { ne }

func neTwoParam(ne: NE, _:Int) -> NE { ne } // OK

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
    get { // OK
      ne
    }

    set { // OK
      ne = newValue
    }
  }

  var neYielded: NE {
    _read { // OK
      yield ne
    }

    @lifetime(borrow self)
    _modify {
      yield &ne
    }
  }

  var neComputedLifetime: NE {
    @lifetime(self) // OK
    get {
      ne
    }

    @lifetime(self) // OK
    set {
      ne = newValue
    }
  }

  var neYieldedLifetime: NE {
    @lifetime(self) // OK
    _read {
      yield ne
    }

    @lifetime(self) // OK
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
    get { // OK
      ne
    }

    set { // OK
      ne = newValue
    }
  }

  var neYielded: NE {
    _read { // OK
      yield ne
    }

    @lifetime(&self)
    _modify {
      yield &ne
    }
  }

  var neComputedLifetime: NE {
    @lifetime(self) // OK
    get {
      ne
    }

    @lifetime(self) // OK
    set {
      ne = newValue
    }
  }

  var neYieldedLifetime: NE {
    @lifetime(self) // OK
    _read {
      yield ne
    }

    @lifetime(self) // OK
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
  mutating func mutatingMethodNoParam() {} // OK

  @lifetime(self: self) // OK
  mutating func mutatingMethodNoParamLifetime() {}

  @lifetime(self: copy self) // OK
  mutating func mutatingMethodNoParamCopy() {}

  @lifetime(self: self) // OK
  mutating func mutatingMethodOneParamLifetime(_: NE) {}

  @lifetime(copy self) // OK
  mutating func mutatingMethodOneParamCopy(_: NE) {}

  @lifetime(borrow self)
  mutating func mutatingMethodOneParamBorrow(_: NE) {}
}
