// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_SuppressedAssociatedTypes

// Coverage testing for LifetimeDependence inferrence logic. The tests are sorted and grouped according to
// docs/ReferenceGuides/LifetimeAnnotation.md. To find the cases that cover the default lifetime
// rules described in the documentation, search for DEFAULT.
//
// Each default case is also defined in Sema/lifetime_depend_infer_defaults.swift to check that the function type has
// the correct dependencies.

class C {}

struct NE: ~Escapable {}

struct NEImmortal: ~Escapable {
  @_lifetime(immortal)
  init() {}
}

struct MutNE: ~Copyable & ~Escapable {}

// =============================================================================
// Same-type default rule
// =============================================================================

/* DEFAULT: @_lifetime(copy ne) */
func sameTypeParam(ne: NE) -> NE { ne }

/* DEFAULT: @_lifetime(copy ne) */
func sameTypeConsumingParam(ne: consuming NE) -> NE { ne }

/* DEFAULT: @_lifetime(copy ne) */
func sameTypeBorrowingParam(ne: borrowing NE) -> NE { ne }

func sameTypeInoutParam(ne: inout NE) -> NE { ne } // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}

/* DEFAULT: @_lifetime(copy ne, copy ne2) */
func sameTypeParam_sameTypeParam(ne1: NE, ne2: NE) -> NE { ne1 }

/* DEFAULT: @_lifetime(copy ne) */
func sameTypeParam_otherTypeParam(ne: NE, c: C) -> NE { ne }

/* DEFAULT: @_lifetime(copy ne) */
func sameTypeParam_sameTypeInoutParam(ne: NE, mutNE: inout NE) -> NE { ne }

struct NonEscapable<T>: ~Escapable {
  @_lifetime(immortal)
  init() {}

  /* DEFAULT: @_lifetime(copy ne) */
  init(ne: Self) {}

  init(c: C) {} // expected-error{{cannot borrow the lifetime of 'c', which has consuming ownership on an initializer}}

  /* DEFAULT: @_lifetime(copy ne) */
  init(ne: Self, c: C) {}

  /* DEFAULT: @_lifetime(copy ne) */
  init<U>(ne: Self, u: U) {}

  /* DEFAULT: @_lifetime(copy neNominal) */
  init<U>(neNominal: NonEscapable<T>, u: U) {}

  init<U>(other: NonEscapable<U>) {} // expected-error{{cannot infer the lifetime dependence scope on an initializer with a ~Escapable parameter, specify '@_lifetime(borrow other)' or '@_lifetime(copy other)'}}

  /* DEFAULT: @_lifetime(copy self) */
  func sameTypeSelf_noParam() -> Self { self }

  /* DEFAULT: @_lifetime(copy self) */
  consuming func sameTypeConsumingSelf_noParam() -> Self { self }

  /* DEFAULT: @_lifetime(copy self) */
  borrowing func sameTypeBorrowingSelf_noParam() -> Self { self }

  mutating func sameTypeMutatingSelf() -> Self { self } // expected-error{{a mutating method with a ~Escapable result requires '@_lifetime(...)'}}

  /* DEFAULT: @_lifetime(copy self, copy ne) */
  func sameTypeSelf_sameTypeParam(ne: Self) -> Self { self }

  /* DEFAULT: @_lifetime(copy self) */
  func sameTypeSelf_otherTypeParam(c: C) -> Self { self }

  /* DEFAULT: @_lifetime(copy self) */
  func sameTypeSelf_sameTypeInoutParam(mutNE: inout Self) -> Self { self }

  /* DEFAULT: @_lifetime(copy self) */
  func sameArchetypeSelf<U>(u: U) -> Self { self }

  /* DEFAULT: @_lifetime(copy self) */
  func sameNominalTypeSelf_genericParam<U>(u: U) -> NonEscapable<T> { self }

  func otherNominalTypeSelf_genericParam<U>(u: U) -> NonEscapable<U> { NonEscapable<U>() } // expected-error{{a method with a ~Escapable result requires '@_lifetime(...)'}}
}

protocol NonEscapableProtocol: ~Escapable {
  @_lifetime(immortal)
  static func create() -> Self

  init(ne: Self)

  init<U>(ne: Self, u: U)

  init(ne: some NonEscapableProtocol, c: C) // expected-error{{an initializer with a ~Escapable result requires '@_lifetime(...)'}}

  init(neProtocol: any NonEscapableProtocol, c: C) // expected-error{{an initializer with a ~Escapable result requires '@_lifetime(...)'}}

  /* DEFAULT: @_lifetime(copy self) */
  func sameArchetypeSelf() -> Self

  mutating func sameArchetypeMutatingSelf() -> Self // expected-error{{a mutating method with a ~Escapable result requires '@_lifetime(...)'}}
}

extension NonEscapableProtocol where Self: ~Escapable {
  func sameArchetypeSelf() -> Self {
    Self.create()
  }
}

/* DEFAULT: @_lifetime(copy ne) */
func sameGenericTypeParam<T: ~Escapable>(ne: T) -> T {
  ne
}

func otherGenericTypeParam<T: ~Escapable,
                           U: NonEscapableProtocol & ~Escapable>(
  ne: T, utype: U.Type) -> U { // expected-error{{a function with a ~Escapable result requires '@_lifetime(...)'}}
  U.create()
}

protocol P_NE {
  associatedtype T: ~Escapable
}

protocol Q_NE {
  associatedtype U: NonEscapableProtocol & ~Escapable
}

struct AssociatedNE<P: P_NE, Q: Q_NE> {
  func otherAssociatedTypeParam(a: P.T, b: P.T) -> Q.U { // expected-error{{a method with a ~Escapable result requires '@_lifetime(...)'}}
    Q.U.create()
  }
}

extension AssociatedNE where P.T == Q.U {
  /* OK: @_lifetime(copy a) is valid and default */
  func sameAssociatedTypeParam(a: P.T) -> Q.U {
    // Note: the same-type rule is satisfied in this context, but the declaration of 'otherAssociatedTypeParam'
    // is itself invalid.
    otherAssociatedTypeParam(a: a, b: a)
  }
}

protocol QQ_NE {
  associatedtype U: NonEscapableProtocol & ~Escapable
}

struct AssociatedQ<Q: Q_NE, QQ: QQ_NE> {}

extension AssociatedQ where Q == QQ {
  static func sameAssociatedTypeParam(a: Q.U) -> QQ.U {
    QQ.U.create()
  }
}

// =============================================================================
// Same type default rule for conditionally Escapable parameters
// =============================================================================

/* DEFAULT: @_lifetime(copy t) */
func unconditionalNESource<T: ~Escapable>(ne: NE, t: T) -> T {
  t
}

struct NE1<T: ~Escapable>: ~Escapable {
  var t: T
}

extension NE1: Escapable where T: Escapable {}

/* DEFAULT: @_lifetime(copy ne) */
func conditionalNESource<T: ~Escapable>(ne: NE1<T>) -> T {
  ne.t
}

/* DEFAULT: @_lifetime(copy t) */
func conditionalNEDest<T: ~Escapable>(t: T) -> NE1<T> {
  NE1(t: t)
}

struct NE2<T: ~Escapable>: ~Escapable {
  var t: T
}

extension NE2: Escapable where T: Escapable {}

/* DEFAULT: @_lifetime(copy ne) */
func conditionalNESourceDest<T: ~Escapable>(ne: NE1<T>) -> NE2<T> {
  NE2(t: ne.t)
}

/* DEFAULT: @_lifetime(copy ne) */
func conditionalNENestedSource<T: ~Escapable>(ne: NE1<NE1<T>>) -> NE2<T> {
  NE2(t: ne.t.t)
}

/* DEFAULT: @_lifetime(copy ne) */
func specializedNESource(ne: NE1<NE>) -> NE { ne.t }

/* DEFAULT: @_lifetime(copy ne) */
func specializedNEDest(ne: NE) -> NE1<NE> { NE1(t: ne) }

/* DEFAULT: @_lifetime(copy ne) */
func specializedNESourceDest(ne: NE1<NE>) -> NE2<NE> {
  NE2<NE>(t: ne.t)
}

/* DEFAULT: @_lifetime(copy ne) */
func optionalUnwrap(ne: NE?) -> NE { ne! }

/* DEFAULT: @_lifetime(copy ne) */
func optionalWrap(ne: NE) -> NE? { ne }

struct NEPair<T: ~Escapable, U: ~Escapable>: ~Escapable {
  var t: T
  var u: U

  @_lifetime(copy t, copy u)
  init(t: T, u: U) {
    self.t = t
    self.u = u
  }
}

extension NEPair: Escapable where T: Escapable, U: Escapable {}

func testNEPair<T: ~Escapable, U: ~Escapable>(ne: NEPair<T, U>) -> T {
  ne.t
}

struct NESpareType<T: ~Escapable, U: ~Escapable>: ~Escapable {
  var t: T
}

extension NESpareType: Escapable where T: Escapable, U: ~Escapable {}

func testNESpareType<T: ~Escapable, U>(ne: NESpareType<T, U>) -> T {
  ne.t
}

// =============================================================================
// Single parameter default rule for functions
// =============================================================================

func noParam_NEResult() -> NEImmortal { NEImmortal() } // expected-error{{a function with a ~Escapable result needs a parameter to depend on}}
// expected-note@-1{{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}

@_lifetime(immortal)
func noParamImmortal_NEResult() -> NEImmortal { NEImmortal() } // OK

/* DEFAULT: @_lifetime(borrow i) */
func oneTrivialParam_NEResult(i: Int) -> NEImmortal { NEImmortal() }

/* DEFAULT: @_lifetime(borrow c) */
func oneParam_NEResult(c: C) -> NEImmortal { NEImmortal() }

/* DEFAULT: @_lifetime(borrow c) */
@_lifetime(c)
func oneParamLifetime_NEResult(c: C) -> NEImmortal { NEImmortal() }

/* DEFAULT: @_lifetime(borrow c) */
func oneParamBorrow_NEResult(c: borrowing C) -> NEImmortal { NEImmortal() } // OK

/* DEFAULT: @_lifetime(borrow c) */
@_lifetime(c)
func oneParamBorrowLifetime_NEResult(c: borrowing C) -> NEImmortal { NEImmortal() } // OK

func oneParamConsume_NEResult(c: consuming C) -> NEImmortal { NEImmortal() } // expected-error{{cannot borrow the lifetime of 'c', which has consuming ownership on a function}}

@_lifetime(c) // expected-error{{invalid lifetime dependence on an Escapable value with consuming ownership}}
func oneParamConsumeLifetime_NEResult(c: consuming C) -> NEImmortal { NEImmortal() }

/* DEFAULT: @_lifetime(&c) */
func oneInoutParam_NEResult(c: inout C) -> NEImmortal { NEImmortal() } // OK

/* DEFAULT: @_lifetime(&c) */
@_lifetime(c)
func oneParamInoutLifetime_NEResult(c: inout C) -> NEImmortal { NEImmortal() } // OK

func twoParams_NEResult(c: C, _: Int) -> NEImmortal { NEImmortal() } // expected-error{{a function with a ~Escapable result requires '@_lifetime(...)'}}

@_lifetime(c)
func twoParamsLifetime_NEResult(c: C, _: Int) -> NEImmortal { NEImmortal() }

func twoParamsConsume_NEResult(c: consuming C, _: Int) -> NEImmortal { NEImmortal() } // expected-error{{a function with a ~Escapable result requires '@_lifetime(...)'}}

func twoParamsBorrow_NEResult(c: borrowing C, _: Int) -> NEImmortal { NEImmortal() } // expected-error{{a function with a ~Escapable result requires '@_lifetime(...)'}}

func twoParamsInout_NEResult(c: inout C, _: Int) -> NEImmortal { NEImmortal() } // expected-error{{a function with a ~Escapable result requires '@_lifetime(...)'}}

func neParam_NEResult(ne: NE) -> NE { ne }

@_lifetime(ne) // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}
func neParamLifetime_NEResult(ne: NE) -> NE { ne }

func neParamBorrow_NEResult(ne: borrowing NE) -> NE { ne }

@_lifetime(ne) // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}
func neParamBorrowLifetime_NEResult(ne: borrowing NE) -> NE { ne }

func neParamConsume_NEResult(ne: consuming NE) -> NE { ne }

@_lifetime(ne) // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}
func neParamConsumeLifetime_NEResult(ne: consuming NE) -> NE { ne }

func neParam_IntParam_NEResult(ne: NE, _:Int) -> NE { ne }

func inoutParam_inoutParam_NEResult(a: inout C, b: inout C) -> NEImmortal { NEImmortal() }
// expected-error@-1{{a function with a ~Escapable result requires '@_lifetime(...)'}}


// =============================================================================
// Single parameter default rule for methods
// =============================================================================

struct EscapableNonTrivialSelf {
  let c: C

  init(c: C) { self.c = c }

  /* DEFAULT: @_lifetime(borrow self) */
  func noParam_NEResult() -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(borrow self) */
  @_lifetime(self)
  func noParamLifetime_NEResult() -> NEImmortal { NEImmortal() }

  @_lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type}}
                        // expected-note@-1{{use '@_lifetime(borrow self)' instead}}
  func noParamCopy_NEResult() -> NEImmortal { NEImmortal() }

  @_lifetime(borrow self)
  func noParamBorrow_NEResult() -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(&self) */
  mutating func mutating_noParam_NEResult() -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(&self) */
  @_lifetime(self)
  mutating func mutating_noParamLifetime_NEResult() -> NEImmortal { NEImmortal() }

  @_lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type}}
                        // expected-note@-1{{use '@_lifetime(&self)' instead}}
  mutating func mutating_noParamCopy_NEResult() -> NEImmortal { NEImmortal() }

  @_lifetime(&self)
  mutating func mutating_noParamBorrow_NEResult() -> NEImmortal { NEImmortal() }

  func oneParam_NEResult(_: Int) -> NEImmortal { NEImmortal() } // expected-error{{a method with a ~Escapable result requires '@_lifetime(...)'}}

  @_lifetime(self)
  func oneParamLifetime_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  @_lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type}}
                        // expected-note@-1{{use '@_lifetime(borrow self)' instead}}
  func oneParamCopy_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  @_lifetime(borrow self)
  func oneParamBorrow_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  mutating func mutating_oneParam_NEResult(_: Int) -> NEImmortal { NEImmortal() } // expected-error{{a mutating method with a ~Escapable result requires '@_lifetime(...)'}}

  /* DEFAULT: @_lifetime(borrow self) */
  @_lifetime(self)
  mutating func mutating_oneParamLifetime_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  @_lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type}}
                        // expected-note@-1{{use '@_lifetime(&self)' instead}}
  mutating func mutating_oneParamCopy_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  @_lifetime(&self)
  mutating func mutating_oneParamBorrow_NEResult(_: Int) -> NEImmortal { NEImmortal() }
}

struct EscapableTrivialSelf {
  func noParam_NEResult() -> NEImmortal { NEImmortal() } // expected-error{{cannot infer lifetime dependence on a method because 'self' is BitwiseCopyable}}

  /* DEFAULT: @_lifetime(borrow self) */
  @_lifetime(self) // OK
  func noParamLifetime_NEResult() -> NEImmortal { NEImmortal() }

  @_lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type}}
                        // expected-note@-1{{use '@_lifetime(borrow self)' instead}}
  func noParamCopy_NEResult() -> NEImmortal { NEImmortal() }

  @_lifetime(borrow self) // OK
  func noParamBorrow_NEResult() -> NEImmortal { NEImmortal() }

  func mutatingMethodNoParam_NEResult() -> NEImmortal { NEImmortal() } // expected-error{{cannot infer lifetime dependence on a method because 'self' is BitwiseCopyable}}

  /* DEFAULT: @_lifetime(borrow self) */
  @_lifetime(self) // OK
  mutating func mutatingMethodNoParamLifetime_NEResult() -> NEImmortal { NEImmortal() }

  @_lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type}}
                        // expected-note@-1{{use '@_lifetime(&self)' instead}}
  mutating func mutatingMethodNoParamCopy_NEResult() -> NEImmortal { NEImmortal() }

  @_lifetime(&self)
  mutating func mutatingMethodNoParamBorrow_NEResult() -> NEImmortal { NEImmortal() }

  func oneParam_NEResult_NEResult(_: Int) -> NEImmortal { NEImmortal() } // expected-error{{a method with a ~Escapable result requires '@_lifetime(...)'}}

  /* DEFAULT: @_lifetime(borrow self) */
  @_lifetime(self)
  func oneParamLifetime_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  @_lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type}}
                        // expected-note@-1{{use '@_lifetime(borrow self)' instead}}
  func oneParamCopy_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  @_lifetime(borrow self)
  func oneParamBorrow_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  mutating func mutating_oneParam_NEResult(_: Int) -> NEImmortal { NEImmortal() } // expected-error{{a mutating method with a ~Escapable result requires '@_lifetime(...)'}}

  /* DEFAULT: @_lifetime(borrow self) */
  @_lifetime(self)
  mutating func mutating_oneParamLifetime_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  @_lifetime(copy self) // expected-error{{cannot copy the lifetime of an Escapable type}}
                        // expected-note@-1{{use '@_lifetime(&self)' instead}}
  mutating func mutating_oneParamCopy_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  @_lifetime(&self)
  mutating func mutating_oneParamBorrow_NEResult(_: Int) -> NEImmortal { NEImmortal() }
}

extension NonEscapable /* where Self: ~Escapable */ {
  func noParam_NEResult() -> NonEscapable { self }

  @_lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
  func noParamLifetime_NEResult() -> NonEscapable { self }

  @_lifetime(copy self) // OK
  func noParamCopy_NEResult_NEResult() -> NonEscapable { self }

  @_lifetime(borrow self) // OK
  func noParamBorrow_NEResult() -> NonEscapable { self }

  mutating func mutating_noParam_NEResult() -> NonEscapable { self } // expected-error{{a mutating method with a ~Escapable result requires '@_lifetime(...)'}}

  @_lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
  mutating func mutating_noParamLifetime_NEResult() -> NonEscapable { self }

  @_lifetime(copy self) // OK
  mutating func mutating_noParamCopy_NEResult() -> NonEscapable { self }

  @_lifetime(&self) // OK
  mutating func mutating_noParamBorrow_NEResult() -> NonEscapable { self }

  func oneParam_NEResult(_: Int) -> NonEscapable { self }

  @_lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
  func oneParamLifetime_NEResult(_: Int) -> NonEscapable { self }

  @_lifetime(copy self) // OK
  func oneParamCopy_NEResult(_: Int) -> NonEscapable { self }

  @_lifetime(borrow self) // OK
  func oneParamBorrow_NEResult(_: Int) -> NonEscapable { self }

  mutating func mutating_oneParam_NEResult(_: Int) -> NonEscapable { self } // expected-error{{a mutating method with a ~Escapable result requires '@_lifetime(...)'}}

  @_lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
  mutating func mutating_oneParamLifetime_NEResult(_: Int) -> NonEscapable { self }

  @_lifetime(copy self) // OK
  mutating func mutating_oneParamCopy_NEResult(_: Int) -> NonEscapable { self }

  @_lifetime(&self) // OK
  mutating func mutating_oneParamBorrow_NEResult(_: Int) -> NonEscapable { self }

  mutating func mutating_inoutParam_NEResult(a: inout NE) -> NEImmortal { NEImmortal() } // expected-error{{a mutating method with a ~Escapable result requires '@_lifetime(...)'}}
}

// =============================================================================
// inout parameter default rule for functions
// =============================================================================

/* DEFAULT: @_lifetime(ne: copy ne) */
func inoutNEParam_void(ne: inout NE) {} // OK

/* DEFAULT: @_lifetime(0: copy 0) */
func inoutNEParam_NEParam_void(_: inout NE, _: NE) {} // OK

/* DEFAULT: @_lifetime(0: copy 0) */
/* DEFAULT: @_lifetime(1: copy 1) */
func inoutParam_inoutNEParam_void(_: inout NE, _: inout NE) {} // OK


func inoutNEParam_NEResult(ne: inout NE) -> NE { ne } // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}

/* DEFAULT: @_lifetime(ne: copy ne) */
@_lifetime(&ne)
func inoutNEParam_NEResult_Lifetime(ne: inout NE) -> NE { ne }

// =============================================================================
// inout parameter default rule for methods
// =============================================================================

extension EscapableNonTrivialSelf {
  /* DEFAULT: @_lifetime(ne: copy ne) */
  func inoutNEParam_void(ne: inout NE) {}

  /* DEFAULT: @_lifetime(ne: copy ne) */
  mutating func mutating_inoutNEParam_void(ne: inout NE) {}

  /* DEFAULT: @_lifetime(ne: copy NE) */
  @_lifetime(&ne)
  func inoutNEParam_NEResult_Lifetime(ne: inout NE) -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(ne: copy NE) */
  @_lifetime(self)
  func inoutNEParam_NEResult_LifetimeSelf(ne: inout NE) -> NEImmortal { NEImmortal() }
}

struct NonEscapableMutableSelf: ~Escapable {
  // This is unambiguous: inout 'self' needs a dependency, and it can't be a borrow dependency because the original
  // value is consumed.
  /* DEFAULT: @_lifetime(self: copy self) */
  mutating func mutating_noParam_void() {} // OK

  @_lifetime(self: self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
  mutating func mutating_lifetime_noParam_void() {}

  @_lifetime(self: copy self) // OK
  mutating func mutating_copyLifetime_noParam_void() {}

  @_lifetime(self: &self) // expected-error{{invalid use of inout dependence on the same inout parameter}}
                          // expected-note @-1{{use '@_lifetime(self: copy self) to forward the inout dependency}}
  mutating func mutating_inoutLifetime_noParam_void() {}

  /* DEFAULT: @_lifetime(self: copy self) */
  mutating func mutating_oneParam_void(_: NE) {}

  @_lifetime(self: self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
  mutating func mutating_lifetime_oneParam(_: NE) {}

  @_lifetime(self: copy self) // OK
  mutating func mutating_copyLifetime_oneParam(_: NE) {}

  /* DEFAULT: @_lifetime(self: copy self) */
  /* DEFAULT: @_lifetime(a: copy a) */
  mutating func mutating_inoutParam_void(ne: inout NE) {}

  mutating func mutating_noParam_NEResult_noLifetime() -> NEImmortal { NEImmortal() } // expected-error{{a mutating method with a ~Escapable result requires '@_lifetime(...)}}

  /* DEFAULT: @_lifetime(self: copy Self) */
  @_lifetime(&self)
  mutating func mutating_noParam_NEResult() -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(self: copy Self) */
  /* DEFAULT: @_lifetime(ne: copy NE) */
  @_lifetime(&self)
  mutating func mutating_inoutNEParam_NEResult(ne: inout NE) -> NEImmortal { NEImmortal() }
}

// =============================================================================
// non-Escapable Initialization
// =============================================================================

// An implicit initializer illegally consumes its nontrivial parameter.
public struct NonescapableImplicitInitializer: ~Escapable {
  // expected-error @-1{{cannot borrow the lifetime of 'c', which has consuming ownership on an implicit initializer}}
  var c: C
}

struct NonescapableInitializers: ~Escapable {
  var c: C

  init() { c = C() } // expected-error{{an initializer with a ~Escapable result needs a parameter to depend on}}
  // expected-note@-1{{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
 
  init(c: C) { self.c = c } // expected-error{{cannot borrow the lifetime of 'c', which has consuming ownership on an initializer}}

  init(c: C, _: Int) { self.c = c } // expected-error{{an initializer with a ~Escapable result requires '@_lifetime(...)'}}

  init(ne: NE) { c = C() } // expected-error{{cannot infer the lifetime dependence scope on an initializer with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}
}

struct NonescapableConsumingInitializers: ~Escapable {
  var c: C // implicit get/set is OK

  init(c: consuming C) { self.c = c } // expected-error{{cannot borrow the lifetime of 'c', which has consuming ownership on an initializer}}

  init(c: consuming C, _: Int) { self.c = c } // expected-error{{an initializer with a ~Escapable result requires '@_lifetime(...)'}}

  init(ne: consuming NE) { c = C() } // expected-error{{cannot infer the lifetime dependence scope on an initializer with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}
}

struct NonescapableBorrowingInitializers: ~Escapable {
  var c: C // implicit stored property set is OK

  init(c: borrowing C) { self.c = c } // OK

  init(c: borrowing C, _: Int) { self.c = c } // expected-error{{an initializer with a ~Escapable result requires '@_lifetime(...)'}}

  init(ne: borrowing NE) { c = C() } // expected-error{{cannot infer the lifetime dependence scope on an initializer with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}
}

struct NonescapableInoutInitializers: ~Escapable {
  var c: C // implicit stored property set is OK

  init(c: inout C) { self.c = c } // OK

  init(c: inout C, _: Int) { self.c = c } // expected-error{{an initializer with a ~Escapable result requires '@_lifetime(...)'}}

  init(ne: inout NE) { c = C() } // expected-error{{cannot infer the lifetime dependence scope on an initializer with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}
}

// =============================================================================
// Aggregate initialization
// =============================================================================

// Motivation: Non-escapable struct definitions often have inicidental integer fields that are unrelated to lifetime.
// Without an explicit initializer, the compiler would infer these fields to be borrowed by the implicit intializer.
// This inevitabely results in lifetime diagnostic errors elsewhere in the code that can't be tracked down at the use
// site:
//
//     let span = CountedSpan(span: span, i: 3) // ERROR: span depends on the lifetime of this value
//
struct CountedSpan: ~Escapable { // expected-error{{cannot infer implicit initialization lifetime. Add an initializer with '@_lifetime(...)' for each parameter the result depends on}}
  let span: Span<Int>
  let i: Int
}

struct NE_Int: ~Escapable {
  let i: Int
}

struct NE_C: ~Escapable { // expected-error{{cannot borrow the lifetime of 'c', which has consuming ownership on an implicit initializer}}
  let c: C
}

struct NE_C_Int: ~Escapable { // expected-error{{cannot infer implicit initialization lifetime. Add an initializer with '@_lifetime(...)' for each parameter the result depends on}}
  let c: C
  let i: Int
}

struct NE_Int_Int: ~Escapable { // expected-error{{cannot infer implicit initialization lifetime. Add an initializer with '@_lifetime(...)' for each parameter the result depends on}}
  let i: Int
  let j: Int
}

struct NE_NE: ~Escapable {
  let ne: NE
}

struct NE_NE_Int: ~Escapable { // expected-error{{cannot infer implicit initialization lifetime. Add an initializer with '@_lifetime(...)' for each parameter the result depends on}}
  let ne: NE
  let i: Int
}

struct NE_NE_C: ~Escapable { // expected-error{{cannot infer implicit initialization lifetime. Add an initializer with '@_lifetime(...)' for each parameter the result depends on}}
  let ne: NE
  let c: C
}

// =============================================================================
// Accessors:
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
    /* DEFAULT: @_lifetime(borrow self) */
    get { // OK
      NEImmortal()
    }

    set { // OK (no dependency)
    }
  }

  var neYielded: NEImmortal {
    /* DEFAULT: @_lifetime(borrow self) */
    _read { // OK
      yield NEImmortal()
    }

    /* DEFAULT: @_lifetime(borrow self) */
    _modify { // OK
      var ne = NEImmortal()
      yield &ne
    }
  }

  // Synthesized _modify...
  subscript(_ index: Int) -> NEImmortal {
    /* DEFAULT: @_lifetime(borrow self) */
    get { // OK
      NEImmortal()
    }

    set { // OK (no dependency)
    }
  }
}

struct TrivialAccessors {
  let p: UnsafeRawPointer

  // The implicit _read/_modify accessors must be inferred. They cannot be written explicitly because a getter is
  // already defined.
  var neComputed: NEImmortal {
    @_lifetime(borrow self)
    get { // OK
      NEImmortal()
    }

    set { // OK (no dependency)
    }
  }

  var neYielded: NEImmortal {
    @_lifetime(borrow self)
    _read { // OK
      yield NEImmortal()
    }

    @_lifetime(&self)
    _modify { // OK
      var ne = NEImmortal()
      yield &ne
    }
  }
}

struct NonescapableSelfAccessors: ~Escapable {
  var ne: NE

  @_lifetime(immortal)
  init() {
    ne = NE()
  }

  var neComputed: NE {
    get { // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
      ne
    }

    // DEFAULT: '@_lifetime(self: copy self, copy newValue)
    set {
      ne = newValue
    }
  }

  var neYielded: NE {
    _read { // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
      yield ne
    }

    @_lifetime(&self)
    _modify {
      yield &ne
    }
  }

  var neComputedLifetime: NE {
    @_lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
    get {
      ne
    }

    @_lifetime(self: newValue) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@_lifetime(borrow newValue)' or '@_lifetime(copy newValue)'}}
    set {
      ne = newValue
    }
  }

  var neYieldedLifetime: NE {
    @_lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
    _read {
      yield ne
    }

    @_lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
    _modify {
      yield &ne
    }
  }

  var neComputedCopy: NE {
    @_lifetime(copy self)
    get {
      ne
    }

    @_lifetime(self: copy newValue)
    set {
      ne = newValue
    }
  }

  var neYieldedCopy: NE {
    @_lifetime(copy self)
    _read {
      yield ne
    }

    @_lifetime(copy self)
    _modify {
      yield &ne
    }
  }

  var neComputedBorrow: NE {
    @_lifetime(borrow self)
    get {
      ne
    }

    @_lifetime(self: borrow newValue)
    set {
      ne = newValue
    }
  }

  var neYieldedBorrow: NE {
    @_lifetime(borrow self)
    _read {
      yield ne
    }

    @_lifetime(&self)
    _modify {
      yield &ne
    }
  }
}

struct NoncopyableSelfAccessors: ~Copyable & ~Escapable {
  var ne: NE

  var neComputed: NE {
    get { // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
      ne
    }

    // DEFAULT: '@_lifetime(self: copy self, copy newValue)
    set {
      ne = newValue
    }
  }

  var neYielded: NE {
    _read { // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
      yield ne
    }

    @_lifetime(&self)
    _modify {
      yield &ne
    }
  }

  var neComputedLifetime: NE {
    @_lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
    get {
      ne
    }

    @_lifetime(self: newValue) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@_lifetime(borrow newValue)' or '@_lifetime(copy newValue)'}}
    set {
      ne = newValue
    }
  }

  var neYieldedLifetime: NE {
    @_lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
    _read {
      yield ne
    }

    @_lifetime(self) // expected-error{{cannot infer the lifetime dependence scope on a mutating method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
    _modify {
      yield &ne
    }
  }

  var neComputedCopy: NE {
    @_lifetime(copy self)
    get {
      ne
    }

    @_lifetime(self: copy newValue)
    set {
      ne = newValue
    }
  }

  var neYieldedCopy: NE {
    @_lifetime(copy self)
    _read {
      yield ne
    }

    @_lifetime(copy self)
    _modify {
      yield &ne
    }
  }

  var neComputedBorrow: NE {
    @_lifetime(borrow self)
    get {
      ne
    }

    @_lifetime(self: copy newValue)
    set {
      ne = newValue
    }
  }

  var neYieldedBorrow: NE {
    @_lifetime(borrow self)
    _read {
      yield ne
    }

    @_lifetime(&self)
    _modify {
      yield &ne
    }
  }
}

// ==================================================================================
// Common mistakes with inout parameter annotations requiring custom diagnostics...
// ==================================================================================

// Invalid keyword for the dependence kind.
//
@_lifetime(a: inout a) // expected-error{{expected 'copy', 'borrow', or '&' followed by an identifier or 'self' in lifetime dependence specifier}}
func f_inout_bad_keyword(a: inout MutableRawSpan) {}

// Don't allow a useless borrow dependency on an inout param--it is misleading.
//
@_lifetime(a: &a) // expected-error{{invalid use of inout dependence on the same inout parameter}}
                  // expected-note @-1{{use '@_lifetime(a: copy a) to forward the inout dependency}}
func f_inout_useless(a: inout MutableRawSpan) {}
