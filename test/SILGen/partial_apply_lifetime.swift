// RUN: %target-swift-emit-silgen -module-name partial_apply_lifetime -enable-experimental-feature Lifetimes %s | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

// These tests exercise the lifetime dependencies computed for partial_apply
// result types by LifetimeDependenceInfo::partialApply. Each case pins down
// exactly what partialApply must produce by declaring an explicit
// @_lifetime(...) on the callee's closure parameter and then checking the
// convert_escape_to_noescape target type (which is the partial_apply's result
// type, minus the @noescape attribute). A convert_function between the
// partial_apply and the consuming apply would mean partialApply disagreed with
// the callee's expected type, so `CHECK-NOT: convert_function` is added as an
// extra guard.

struct NE: ~Escapable {
  @_lifetime(immortal)
  init() {}
}

// Baseline: a single-capture closure with a borrow-on-capture result.

@_lifetime(copy f)
func copyNE(f: () -> NE) -> NE {
  f()
}

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime9callGetNE3ne1AA0F0VAE_tF : $@convention(thin) (@guaranteed NE) -> @lifetime(copy 0) @owned NE {
// CHECK: [[FNREF:%[0-9]+]] = function_ref @$s22partial_apply_lifetime9callGetNE3ne1AA0F0VAE_tFAEyXEfU_ : $@convention(thin) (@guaranteed NE) -> @lifetime(borrow 0) @owned NE
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[FNREF]]
// CHECK-NOT: convert_function
// CHECK: [[NECLOSURE:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE]] to $@noescape @callee_guaranteed () -> @lifetime(captures) @owned NE
// CHECK: [[GETNE:%[0-9]+]] = function_ref @$s22partial_apply_lifetime6copyNE1fAA0E0VAEyXE_tF
// CHECK: apply [[GETNE]]([[NECLOSURE]])
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime9callGetNE3ne1AA0F0VAE_tF'
func callGetNE(ne1: NE) -> NE {
  copyNE { ne1 }
}

// Dependency on an unbound formal parameter: the source index is kept and no
// captures flag is added.

@_lifetime(copy f)
func eatOneBorrow(f: @_lifetime(borrow ne) (_ ne: NE) -> NE) -> NE {
  let local = NE()
  return f(local)
}

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime19callBorrowOnUnbound4condAA2NEVSb_tF :
// CHECK: [[FNREF:%[0-9]+]] = function_ref @$s22partial_apply_lifetime19callBorrowOnUnbound4condAA2NEVSb_tFA2EXEfU_ : $@convention(thin) (@guaranteed NE, Bool) -> @lifetime(borrow 0) @owned NE
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[FNREF]]
// CHECK-NOT: convert_function
// CHECK: convert_escape_to_noescape [not_guaranteed] [[CLOSURE]] to $@noescape @callee_guaranteed (@guaranteed NE) -> @lifetime(borrow 0) @owned NE
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime19callBorrowOnUnbound4condAA2NEVSb_tF'
func callBorrowOnUnbound(cond: Bool) -> NE {
  eatOneBorrow { n in if cond { return n } else { return n } }
}

// Dependency on a borrowed source: replaced with captures.

@_lifetime(copy f)
func eatOneCaptures(f: @_lifetime(captures) (NE) -> NE) -> NE {
  let local = NE()
  return f(local)
}

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime14callDepOnBound5boundAA2NEVAE_tF :
// CHECK: [[FNREF:%[0-9]+]] = function_ref @$s22partial_apply_lifetime14callDepOnBound5boundAA2NEVAE_tFA2EXEfU_ : $@convention(thin) (@guaranteed NE, @guaranteed NE) -> @lifetime(borrow 1) @owned NE
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[FNREF]]
// CHECK-NOT: convert_function
// CHECK: convert_escape_to_noescape [not_guaranteed] [[CLOSURE]] to $@noescape @callee_guaranteed (@guaranteed NE) -> @lifetime(captures) @owned NE
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime14callDepOnBound5boundAA2NEVAE_tF'
@_lifetime(copy bound)
func callDepOnBound(bound: NE) -> NE {
  eatOneCaptures { _ in bound }
}

// Dependency on a mix of captured and uncaptured sources: only the dependencies
// on captured parameters (those bound by the partial apply) are replaced with
// the captures dependency source.

@_lifetime(copy f)
func eatOneCapturesAndBorrow(f: @_lifetime(captures, borrow ne) (_ ne: NE) -> NE) -> NE {
  let local = NE()
  return f(local)
}

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime9callMixed5bound4condAA2NEVAF_SbtF :
// CHECK: [[FNREF:%[0-9]+]] = function_ref @$s22partial_apply_lifetime9callMixed5bound4condAA2NEVAF_SbtFA2FXEfU_ : $@convention(thin) (@guaranteed NE, Bool, @guaranteed NE) -> @lifetime(borrow 0, borrow 1, borrow 2) @owned NE
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[FNREF]]
// CHECK-NOT: convert_function
// CHECK: convert_escape_to_noescape [not_guaranteed] [[CLOSURE]] to $@noescape @callee_guaranteed (@guaranteed NE) -> @lifetime(captures, borrow 0) @owned NE
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime9callMixed5bound4condAA2NEVAF_SbtF'
@_lifetime(copy bound)
func callMixed(bound: NE, cond: Bool) -> NE {
  eatOneCapturesAndBorrow { n in cond ? n : bound }
}

// Multiple parameters, all captured.

@_lifetime(copy f)
func eatZeroCaptures(f: @_lifetime(captures) () -> NE) -> NE {
  f()
}

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime18callBindMultiBound1a1b4condAA2NEVAG_AGSbtF :
// CHECK: [[FNREF:%[0-9]+]] = function_ref @$s22partial_apply_lifetime18callBindMultiBound1a1b4condAA2NEVAG_AGSbtFAGyXEfU_ : $@convention(thin) (Bool, @guaranteed NE, @guaranteed NE) -> @lifetime(borrow 0, borrow 1, borrow 2) @owned NE
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[FNREF]]
// CHECK-NOT: convert_function
// CHECK: convert_escape_to_noescape [not_guaranteed] [[CLOSURE]] to $@noescape @callee_guaranteed () -> @lifetime(captures) @owned NE
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime18callBindMultiBound1a1b4condAA2NEVAG_AGSbtF'
@_lifetime(copy a, copy b)
func callBindMultiBound(a: NE, b: NE, cond: Bool) -> NE {
  eatZeroCaptures { cond ? a : b }
}

// Multiple parameters: some captured, some not.

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime20callBindMultiUnbound4cond3tagAA2NEVSb_SitF :
// CHECK: [[FNREF:%[0-9]+]] = function_ref @$s22partial_apply_lifetime20callBindMultiUnbound4cond3tagAA2NEVSb_SitFA2FXEfU_ : $@convention(thin) (@guaranteed NE, Bool, Int) -> @lifetime(borrow 0) @owned NE
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[FNREF]]
// CHECK-NOT: convert_function
// CHECK: convert_escape_to_noescape [not_guaranteed] [[CLOSURE]] to $@noescape @callee_guaranteed (@guaranteed NE) -> @lifetime(borrow 0) @owned NE
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime20callBindMultiUnbound4cond3tagAA2NEVSb_SitF'
@_lifetime(immortal)
func callBindMultiUnbound(cond: Bool, tag: Int) -> NE {
  eatOneBorrow { n in
    if cond && tag > 0 { return n } else { return n }
  }
}

// No lifetime sources: unaffected.

@_lifetime(copy f)
func eatImmortal(f: @_lifetime(immortal) () -> NE) -> NE {
  f()
}

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime12callImmortal5extraAA2NEVSi_tF :
// CHECK: [[FNREF:%[0-9]+]] = function_ref @$s22partial_apply_lifetime12callImmortal5extraAA2NEVSi_tFAEyXEfU_ : $@convention(thin) (Int) -> @lifetime(immortal) @owned NE
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[FNREF]]
// CHECK-NOT: convert_function
// CHECK: convert_escape_to_noescape [not_guaranteed] [[CLOSURE]] to $@noescape @callee_guaranteed () -> @lifetime(immortal) @owned NE
// CHECK-NOT: captures
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime12callImmortal5extraAA2NEVSi_tF'
@_lifetime(immortal)
func callImmortal(extra: Int) -> NE {
  eatImmortal { let _ = extra; return NE() }
}

// Mixed dependency kinds (inherit + scope) within a single entry.

@_lifetime(copy f)
func eatOneCapturesCopy(f: @_lifetime(captures, copy ne) (_ ne: NE) -> NE) -> NE {
  let local = NE()
  return f(local)
}

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime14callMixedKinds5bound4condAA2NEVAF_SbtF :
// CHECK: [[FNREF:%[0-9]+]] = function_ref @$s22partial_apply_lifetime14callMixedKinds5bound4condAA2NEVAF_SbtFA2FXEfU_ : $@convention(thin) (@guaranteed NE, Bool, @guaranteed NE) -> @lifetime(copy 0, borrow 1, borrow 2) @owned NE
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[FNREF]]
// CHECK-NOT: convert_function
// CHECK: convert_escape_to_noescape [not_guaranteed] [[CLOSURE]] to $@noescape @callee_guaranteed (@guaranteed NE) -> @lifetime(captures, copy 0) @owned NE
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime14callMixedKinds5bound4condAA2NEVAF_SbtF'
@_lifetime(copy bound)
func callMixedKinds(bound: NE, cond: Bool) -> NE {
  eatOneCapturesCopy { n in cond ? n : bound }
}

// Converting a non-throwing function to typed-throws: this legitimately requires a
// convert_function.

func takeTypedThrowingNEReturning<E: Error>(_ f: (NE) throws(E) -> NE) {}

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime34reabstractToTypedThrowsNEReturningyyAA2NEVADXEF :
// CHECK: [[THUNK:%[0-9]+]] = function_ref @$s22partial_apply_lifetime2NEVACIggo_A2Cs5NeverOIeggozr_TR : $@convention(thin) (@guaranteed NE, @lifetime(captures, copy 0) @guaranteed @noescape @callee_guaranteed (@guaranteed NE) -> @lifetime(captures, copy 0) @owned NE) -> (@owned NE, @error_indirect Never)
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]
// CHECK: convert_function [[CLOSURE]] to $@callee_guaranteed @substituted <τ_0_0> (@guaranteed NE) -> @lifetime(captures, copy 0) (@owned NE, @error_indirect τ_0_0) for <Never>
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime34reabstractToTypedThrowsNEReturningyyAA2NEVADXEF'
func reabstractToTypedThrowsNEReturning(_ f: (NE) -> NE) {
  takeTypedThrowingNEReturning(f)
}

// Typed throws with inout parameter.

func takeTypedThrowingInout<E: Error>(_ f: (inout NE) throws(E) -> Void) {}

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime28reabstractToTypedThrowsInoutyyyAA2NEVzXEF :
// CHECK: [[THUNK:%[0-9]+]] = function_ref @$s22partial_apply_lifetime2NEVIgl_ACs5NeverOIeglzr_TR : $@convention(thin) (@lifetime(copy 0) @inout NE, @guaranteed @noescape @callee_guaranteed (@lifetime(copy 0) @inout NE) -> ()) -> @error_indirect Never
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]
// CHECK: convert_function [[CLOSURE]] to $@callee_guaranteed @substituted <τ_0_0> (@lifetime(copy 0) @inout NE) -> @error_indirect τ_0_0 for <Never>
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime28reabstractToTypedThrowsInoutyyyAA2NEVzXEF'
func reabstractToTypedThrowsInout(_ f: (inout NE) -> Void) {
  takeTypedThrowingInout(f)
}

// Inout parameter, without a legitimate need for a convert_function:
// No convert_function is emitted.

struct Holder: ~Escapable {
  @_lifetime(immortal)
  init() {}

  @_lifetime(self: copy other)
  mutating func mut(other: NE) {}
}

func consumeInout(_ f: (inout Holder) -> ()) {}

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime13driveMutation5otheryAA2NEV_tF :
// CHECK: [[FNREF:%[0-9]+]] = function_ref @$s22partial_apply_lifetime13driveMutation5otheryAA2NEV_tFyAA6HolderVzXEfU_ : $@convention(thin) (@lifetime(copy 0) @inout Holder, @guaranteed NE) -> ()
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[FNREF]]
// CHECK-NOT: convert_function
// CHECK: convert_escape_to_noescape [not_guaranteed] [[CLOSURE]] to $@noescape @callee_guaranteed (@lifetime(copy 0) @inout Holder) -> ()
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime13driveMutation5otheryAA2NEV_tF'
func driveMutation(other: NE) {
  consumeInout { (h: inout Holder) in h.mut(other: other) }
}

// Captured inout parameter.

// CHECK-LABEL: sil hidden [ossa] @$s22partial_apply_lifetime21callWithCapturedInoutyAA2NEVADzF : $@convention(thin) (@lifetime(copy 0) @inout NE) -> @lifetime(borrow 0) @owned NE {
// CHECK: [[FNREF:%[0-9]+]] = function_ref @$s22partial_apply_lifetime21callWithCapturedInoutyAA2NEVADzFADyXEfU_ : $@convention(thin) (@inout_aliasable NE) -> @lifetime(borrow 0) @owned NE
// CHECK: [[CLOSURE:%[0-9]+]] = partial_apply [callee_guaranteed] [[FNREF]]
// CHECK-NOT: convert_function
// CHECK: convert_escape_to_noescape [not_guaranteed] [[CLOSURE]] to $@noescape @callee_guaranteed () -> @lifetime(captures) @owned NE
// CHECK-LABEL: } // end sil function '$s22partial_apply_lifetime21callWithCapturedInoutyAA2NEVADzF'
@_lifetime(&ne)
func callWithCapturedInout(_ ne: inout NE) -> NE {
  copyNE { ne }
}
