// RUN: %target-swift-emit-silgen -enable-experimental-feature Lifetimes -primary-file %s | %FileCheck %s
//
// -primary-file is required to synthesize accessors.

// REQUIRES: swift_feature_Lifetimes

// Check that all the DEFAULT cases in Sema/lifetime_depend_infer_defaults.swift generate the correct function
// signature. Checking the SILGen output seems like the easiest way.

class C {}

struct NE: ~Escapable {}

struct NEImmortal: ~Escapable {
  @_lifetime(immortal)
  init() {}
}

struct MutNE: ~Copyable & ~Escapable {}

// =============================================================================
// Single parameter default rule for functions
// =============================================================================

/* DEFAULT: @_lifetime(borrow i) */
// CHECK: @$s30lifetime_depend_infer_defaults24oneTrivialParam_NEResult1iAA10NEImmortalVSi_tF : $@convention(thin) (Int) -> @lifetime(borrow 0) @owned NEImmortal
func oneTrivialParam_NEResult(i: Int) -> NEImmortal { NEImmortal() }

/* DEFAULT: @_lifetime(borrow c) */
// CHECK: @$s30lifetime_depend_infer_defaults17oneParam_NEResult1cAA10NEImmortalVAA1CC_tF : $@convention(thin) (@guaranteed C) -> @lifetime(borrow 0) @owned NEImmortal
func oneParam_NEResult(c: C) -> NEImmortal { NEImmortal() }

/* DEFAULT: @_lifetime(borrow c) */
// CHECK: @$s30lifetime_depend_infer_defaults25oneParamLifetime_NEResult1cAA10NEImmortalVAA1CC_tF : $@convention(thin) (@guaranteed C) -> @lifetime(borrow 0) @owned NEImmortal
@_lifetime(c)
func oneParamLifetime_NEResult(c: C) -> NEImmortal { NEImmortal() }

/* DEFAULT: @_lifetime(borrow c) */
// CHECK: @$s30lifetime_depend_infer_defaults23oneParamBorrow_NEResult1cAA10NEImmortalVAA1CC_tF : $@convention(thin) (@guaranteed C) -> @lifetime(borrow 0) @owned NEImmortal
func oneParamBorrow_NEResult(c: borrowing C) -> NEImmortal { NEImmortal() } // OK

/* DEFAULT: @_lifetime(borrow c) */
// CHECK: @$s30lifetime_depend_infer_defaults31oneParamBorrowLifetime_NEResult1cAA10NEImmortalVAA1CC_tF : $@convention(thin) (@guaranteed C) -> @lifetime(borrow 0) @owned NEImmortal
@_lifetime(c)
func oneParamBorrowLifetime_NEResult(c: borrowing C) -> NEImmortal { NEImmortal() } // OK

/* DEFAULT: @_lifetime(&c) */
// CHECK: @$s30lifetime_depend_infer_defaults22oneInoutParam_NEResult1cAA10NEImmortalVAA1CCz_tF : $@convention(thin) (@inout C) -> @lifetime(borrow 0) @owned NEImmortal
func oneInoutParam_NEResult(c: inout C) -> NEImmortal { NEImmortal() } // OK

/* DEFAULT: @_lifetime(&c) */
// CHECK: @$s30lifetime_depend_infer_defaults30oneParamInoutLifetime_NEResult1cAA10NEImmortalVAA1CCz_tF : $@convention(thin) (@inout C) -> @lifetime(borrow 0) @owned NEImmortal
@_lifetime(c)
func oneParamInoutLifetime_NEResult(c: inout C) -> NEImmortal { NEImmortal() } // OK

// =============================================================================
// Single parameter default rule for methods
// =============================================================================

struct EscapableNonTrivialSelf {
  let c: C

  init(c: C) { self.c = c }

  /* DEFAULT: @_lifetime(borrow self) */
  // CHECK: @$s30lifetime_depend_infer_defaults23EscapableNonTrivialSelfV16noParam_NEResultAA10NEImmortalVyF : $@convention(method) (@guaranteed EscapableNonTrivialSelf) -> @lifetime(borrow 0) @owned NEImmortal
  func noParam_NEResult() -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(borrow self) */
  // CHECK: @$s30lifetime_depend_infer_defaults23EscapableNonTrivialSelfV24noParamLifetime_NEResultAA10NEImmortalVyF : $@convention(method) (@guaranteed EscapableNonTrivialSelf) -> @lifetime(borrow 0) @owned NEImmortal
  @_lifetime(self)
  func noParamLifetime_NEResult() -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(&self) */
  // CHECK: @$s30lifetime_depend_infer_defaults23EscapableNonTrivialSelfV25mutating_noParam_NEResultAA10NEImmortalVyF : $@convention(method) (@inout EscapableNonTrivialSelf) -> @lifetime(borrow 0) @owned NEImmortal
  mutating func mutating_noParam_NEResult() -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(&self) */
  // CHECK: @$s30lifetime_depend_infer_defaults23EscapableNonTrivialSelfV33mutating_noParamLifetime_NEResultAA10NEImmortalVyF : $@convention(method) (@inout EscapableNonTrivialSelf) -> @lifetime(borrow 0) @owned NEImmortal
  @_lifetime(self)
  mutating func mutating_noParamLifetime_NEResult() -> NEImmortal { NEImmortal() }
}

struct EscapableTrivialSelf {
  /* DEFAULT: @_lifetime(borrow self) */
  // CHECK: @$s30lifetime_depend_infer_defaults20EscapableTrivialSelfV24noParamLifetime_NEResultAA10NEImmortalVyF : $@convention(method) (EscapableTrivialSelf) -> @lifetime(borrow 0) @owned NEImmortal
  @_lifetime(self) // OK
  func noParamLifetime_NEResult() -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(borrow self) */
  // CHECK: @$s30lifetime_depend_infer_defaults20EscapableTrivialSelfV38mutatingMethodNoParamLifetime_NEResultAA10NEImmortalVyF : $@convention(method) (@inout EscapableTrivialSelf) -> @lifetime(borrow 0) @owned NEImmortal
  @_lifetime(self) // OK
  mutating func mutatingMethodNoParamLifetime_NEResult() -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(borrow self) */
  // CHECK: @$s30lifetime_depend_infer_defaults20EscapableTrivialSelfV25oneParamLifetime_NEResultyAA10NEImmortalVSiF : $@convention(method) (Int, EscapableTrivialSelf) -> @lifetime(borrow 1) @owned NEImmortal
  @_lifetime(self)
  func oneParamLifetime_NEResult(_: Int) -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(borrow self) */
  // CHECK: @$s30lifetime_depend_infer_defaults20EscapableTrivialSelfV34mutating_oneParamLifetime_NEResultyAA10NEImmortalVSiF : $@convention(method) (Int, @inout EscapableTrivialSelf) -> @lifetime(borrow 1) @owned NEImmortal
  @_lifetime(self)
  mutating func mutating_oneParamLifetime_NEResult(_: Int) -> NEImmortal { NEImmortal() }
}

// =============================================================================
// inout parameter default rule for functions
// =============================================================================

/* DEFAULT: @_lifetime(ne: copy ne) */
// CHECK: @$s30lifetime_depend_infer_defaults17inoutNEParam_void2neyAA2NEVz_tF : $@convention(thin) (@lifetime(copy 0) @inout NE) -> ()
func inoutNEParam_void(ne: inout NE) {} // OK

/* DEFAULT: @_lifetime(0: copy 0) */
// CHECK: @$s30lifetime_depend_infer_defaults013inoutNEParam_F5_voidyyAA2NEVz_ADtF : $@convention(thin) (@lifetime(copy 0) @inout NE, @guaranteed NE) -> ()
func inoutNEParam_NEParam_void(_: inout NE, _: NE) {} // OK

/* DEFAULT: @_lifetime(0: copy 0) */
/* DEFAULT: @_lifetime(1: copy 1) */
// CHECK: @$s30lifetime_depend_infer_defaults011inoutParam_E12NEParam_voidyyAA2NEVz_ADztF : $@convention(thin) (@lifetime(copy 0) @inout NE, @lifetime(copy 1) @inout NE) -> ()
func inoutParam_inoutNEParam_void(_: inout NE, _: inout NE) {} // OK

/* DEFAULT: @_lifetime(ne: copy ne) */
// CHECK: @$s30lifetime_depend_infer_defaults30inoutNEParam_NEResult_Lifetime2neAA2NEVAEz_tF : $@convention(thin) (@lifetime(copy 0) @inout NE) -> @lifetime(borrow 0) @owned NE
@_lifetime(&ne)
func inoutNEParam_NEResult_Lifetime(ne: inout NE) -> NE { ne }

// =============================================================================
// inout parameter default rule for methods
// =============================================================================

extension EscapableNonTrivialSelf {
  /* DEFAULT: @_lifetime(ne: copy ne) */
  // CHECK: @$s30lifetime_depend_infer_defaults23EscapableNonTrivialSelfV17inoutNEParam_void2neyAA2NEVz_tF : $@convention(method) (@lifetime(copy 0) @inout NE, @guaranteed EscapableNonTrivialSelf) -> ()
  func inoutNEParam_void(ne: inout NE) {}

  /* DEFAULT: @_lifetime(ne: copy ne) */
  // CHECK: @$s30lifetime_depend_infer_defaults23EscapableNonTrivialSelfV26mutating_inoutNEParam_void2neyAA2NEVz_tF : $@convention(method) (@lifetime(copy 0) @inout NE, @inout EscapableNonTrivialSelf) -> ()
  mutating func mutating_inoutNEParam_void(ne: inout NE) {}

  /* DEFAULT: @_lifetime(ne: copy NE) */
  // CHECK: @$s30lifetime_depend_infer_defaults23EscapableNonTrivialSelfV30inoutNEParam_NEResult_Lifetime2neAA10NEImmortalVAA2NEVz_tF : $@convention(method) (@lifetime(copy 0) @inout NE, @guaranteed EscapableNonTrivialSelf) -> @lifetime(borrow 0) @owned NEImmortal
  @_lifetime(&ne)
  func inoutNEParam_NEResult_Lifetime(ne: inout NE) -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(ne: copy NE) */
  // CHECK: @$s30lifetime_depend_infer_defaults23EscapableNonTrivialSelfV030inoutNEParam_NEResult_LifetimeH02neAA10NEImmortalVAA2NEVz_tF : $@convention(method) (@lifetime(copy 0) @inout NE, @guaranteed EscapableNonTrivialSelf) -> @lifetime(borrow 1) @owned NEImmortal
  @_lifetime(self)
  func inoutNEParam_NEResult_LifetimeSelf(ne: inout NE) -> NEImmortal { NEImmortal() }
}

struct NonEscapableMutableSelf: ~Escapable {
  // This is unambiguous: inout 'self' needs a dependency, and it can't be a borrow dependency because the original
  // value is consumed.
  /* DEFAULT: @_lifetime(self: copy self) */
  // CHECK: @$s30lifetime_depend_infer_defaults23NonEscapableMutableSelfV21mutating_noParam_voidyyF : $@convention(method) (@lifetime(copy 0) @inout NonEscapableMutableSelf) -> ()
  mutating func mutating_noParam_void() {} // OK

  /* DEFAULT: @_lifetime(self: copy self) */
  // CHECK: @$s30lifetime_depend_infer_defaults23NonEscapableMutableSelfV22mutating_oneParam_voidyyAA2NEVF : $@convention(method) (@guaranteed NE, @lifetime(copy 1) @inout NonEscapableMutableSelf) -> ()
  mutating func mutating_oneParam_void(_: NE) {}

  /* DEFAULT: @_lifetime(self: copy self) */
  /* DEFAULT: @_lifetime(a: copy a) */
  // CHECK: @$s30lifetime_depend_infer_defaults23NonEscapableMutableSelfV24mutating_inoutParam_void2neyAA2NEVz_tF : $@convention(method) (@lifetime(copy 0) @inout NE, @lifetime(copy 1) @inout NonEscapableMutableSelf) -> ()
  mutating func mutating_inoutParam_void(ne: inout NE) {}

  /* DEFAULT: @_lifetime(self: copy Self) */
  // CHECK: @$s30lifetime_depend_infer_defaults23NonEscapableMutableSelfV25mutating_noParam_NEResultAA10NEImmortalVyF : $@convention(method) (@lifetime(copy 0) @inout NonEscapableMutableSelf) -> @lifetime(borrow 0) @owned NEImmortal
  @_lifetime(&self)
  mutating func mutating_noParam_NEResult() -> NEImmortal { NEImmortal() }

  /* DEFAULT: @_lifetime(self: copy Self) */
  /* DEFAULT: @_lifetime(ne: copy NE) */
  // CHECK: @$s30lifetime_depend_infer_defaults23NonEscapableMutableSelfV30mutating_inoutNEParam_NEResult2neAA10NEImmortalVAA2NEVz_tF : $@convention(method) (@lifetime(copy 0) @inout NE, @lifetime(copy 1) @inout NonEscapableMutableSelf) -> @lifetime(borrow 1) @owned NEImmortal
  @_lifetime(&self)
  mutating func mutating_inoutNEParam_NEResult(ne: inout NE) -> NEImmortal { NEImmortal() }
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
    // CHECK: @$s30lifetime_depend_infer_defaults9AccessorsV10neComputedAA10NEImmortalVvg : $@convention(method) (@guaranteed Accessors) -> @lifetime(borrow 0) @owned NEImmortal
    get { // OK
      NEImmortal()
    }

    // CHECK: @$s30lifetime_depend_infer_defaults9AccessorsV10neComputedAA10NEImmortalVvs : $@convention(method) (@owned NEImmortal, @inout Accessors) -> ()
    set { // OK (no dependency)
    }
  }

  var neYielded: NEImmortal {
    /* DEFAULT: @_lifetime(borrow self) */
    // CHECK: @$s30lifetime_depend_infer_defaults9AccessorsV9neYieldedAA10NEImmortalVvr : $@yield_once @convention(method) (@guaranteed Accessors) -> @lifetime(borrow 0) @yields @guaranteed NEImmortal
    _read { // OK
      yield NEImmortal()
    }

    /* DEFAULT: @_lifetime(borrow self) */
    // CHECK: @$s30lifetime_depend_infer_defaults9AccessorsV9neYieldedAA10NEImmortalVvM : $@yield_once @convention(method) (@inout Accessors) -> @lifetime(borrow 0) @yields @inout NEImmortal
    _modify { // OK
      var ne = NEImmortal()
      yield &ne
    }
  }

  subscript(_ index: Int) -> NEImmortal {
    /* DEFAULT: @_lifetime(borrow self) */
    // CHECK: @$s30lifetime_depend_infer_defaults9AccessorsVyAA10NEImmortalVSicig : $@convention(method) (Int, @guaranteed Accessors) -> @lifetime(borrow 1) @owned NEImmortal
    get { // OK
      NEImmortal()
    }

    // CHECK: @$s30lifetime_depend_infer_defaults9AccessorsVyAA10NEImmortalVSicis : $@convention(method) (@owned NEImmortal, Int, @inout Accessors) -> ()
    set { // OK (no dependency)
    }

    // Synthesized modify...
    /* DEFAULT: @_lifetime(borrow self) */
    // CHECK: @$s30lifetime_depend_infer_defaults9AccessorsVyAA10NEImmortalVSiciM : $@yield_once @convention(method) (Int, @inout Accessors) -> @lifetime(borrow 1) @yields @inout NEImmortal
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

    // CHECK: @$s30lifetime_depend_infer_defaults16TrivialAccessorsV10neComputedAA10NEImmortalVvs : $@convention(method) (@owned NEImmortal, @inout TrivialAccessors) -> ()
    set { // OK (no dependency)
    }

    // synthesized modify
    // CHECK: @$s30lifetime_depend_infer_defaults16TrivialAccessorsV10neComputedAA10NEImmortalVvM : $@yield_once @convention(method) (@inout TrivialAccessors) -> @lifetime(borrow 0) @yields @inout NEImmortal
  }
}

struct NonescapableSelfAccessors: ~Escapable {
  var ne: NE

  @_lifetime(immortal)
  init() {
    ne = NE()
  }

  var neComputed: NE {
    @_lifetime(copy self)
    get {
      ne
    }
    // DEFAULT: '@_lifetime(self: copy self, copy newValue)
    // CHECK: @$s30lifetime_depend_infer_defaults25NonescapableSelfAccessorsV10neComputedAA2NEVvs : $@convention(method) (@owned NE, @lifetime(copy 0, copy 1) @inout NonescapableSelfAccessors) -> ()
    set {
      ne = newValue
    }

    // synthesized modify.
    // CHECK: @$s30lifetime_depend_infer_defaults25NonescapableSelfAccessorsV10neComputedAA2NEVvM : $@yield_once @convention(method) (@lifetime(copy 0) @inout NonescapableSelfAccessors) -> @lifetime(copy 0) @yields @inout NE
  }
}

struct NoncopyableSelfAccessors: ~Copyable & ~Escapable {
  var ne: NE

  var neComputed: NE {
    @_lifetime(copy self)
    get {
      ne
    }
    // DEFAULT: '@_lifetime(self: copy self, copy newValue)
    // CHECK: @$s30lifetime_depend_infer_defaults24NoncopyableSelfAccessorsV10neComputedAA2NEVvs : $@convention(method) (@owned NE, @lifetime(copy 0, copy 1) @inout NoncopyableSelfAccessors) -> ()
    set {
      ne = newValue
    }

    // synthesized modify.
    // CHECK: @$s30lifetime_depend_infer_defaults24NoncopyableSelfAccessorsV10neComputedAA2NEVvM : $@yield_once @convention(method) (@lifetime(copy 0) @inout NoncopyableSelfAccessors) -> @lifetime(copy 0) @yields @inout NE
  }

}
