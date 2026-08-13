// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -Xllvm -sil-print-types %s -enable-callee-allocated-coro-abi -enable-library-evolution -enable-experimental-feature CoroutineAccessors

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types   \
// RUN:     %s                                              \
// RUN:     -enable-callee-allocated-coro-abi               \
// RUN:     -enable-library-evolution                       \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi-stability

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types                      \
// RUN:     %s                                                                 \
// RUN:     -enable-callee-allocated-coro-abi                                  \
// RUN:     -enable-library-evolution                                          \
// RUN:     -enable-experimental-feature CoroutineAccessors                    \
// RUN:     -enable-experimental-feature CoroutineAccessorsUnwindOnCallerError \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi-stability

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_CoroutineAccessorsUnwindOnCallerError

// A read requirement may be satisfied by
// - a stored property
// - a _read accessor
// - a yielding borrow accessor
// - a get accessor
// - an unsafeAddress accessor

// TODO: CoroutineAccessors: Replace SwiftStdlib 9999 with SwiftStdlib X.Y.

@frozen
public struct U : ~Copyable {}

// Protocols are split up to improve the ordering of the functions in the output
// (implementation, then conformance thunk).
public protocol P1 : ~Copyable {
  @_borrowed
  var ubgs: U { get set }

// A `{ get set }` protocol requirement builds default yielding_borrow/yielding_mutate
// thunks that dispatch to conformer-provided _read/_modify implementations.
// New conformers override these with the real implementations and provide
// _read/_modify thunks.

// CHECK-LABEL: sil {{.*}} [ossa] @$s17read_requirements2P1P4ubgsAA1UVvy : {{.*}} {
// CHECK:      bb0(
// CHECK-SAME:     [[SELF_UNCHECKED:%[^:]+]]
// CHECK-SAME: ):
// CHECK:        [[SELF:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[SELF_UNCHECKED]]
// CHECK:        [[READER:%[^,]+]] = witness_method $Self, #P1.ubgs!read
// CHECK:        ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[READER]]<Self>([[SELF]])
// CHECK:        [[VALUE_COPY_UNCHECKED:%[^,]+]] = copy_value [[VALUE]]
// CHECK:        [[VALUE_COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[VALUE_COPY_UNCHECKED]]
// CHECK:        [[VALUE_BORROW:%[^,]+]] = begin_borrow [[VALUE_COPY]]
// CHECK:        yield [[VALUE_BORROW:%[^,]+]]
// CHECK-SAME:            resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:            unwind [[FAILURE:bb[0-9]+]]
// CHECK:      [[SUCCESS]]:
// CHECK:        end_borrow [[VALUE_BORROW]]
// CHECK:        destroy_value [[VALUE_COPY]]
// CHECK:        end_apply [[TOKEN]]
// CHECK:      [[FAILURE]]:
// CHECK:        end_borrow [[VALUE_BORROW]]
// CHECK:        destroy_value [[VALUE_COPY]]
// CHECK:        end_apply [[TOKEN]]
// CHECK:        unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements2P1P4ubgsAA1UVvy'

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements2P1P4ubgsAA1UVvx : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_UNCHECKED:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[SELF_UNCHECKED]]
// CHECK:         [[SELF_ACCESS:%[^,]+]] = begin_access [modify] [unknown] [[SELF]]
// CHECK:         [[MODIFIER:%[^,]+]] = witness_method $Self, #P1.ubgs!modify
// CHECK:         ([[VALUE_UNCHECKED:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[MODIFIER]]<Self>([[SELF_ACCESS]])
// CHECK:         [[VALUE:%[^,]+]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[VALUE_UNCHECKED]]
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         end_access [[SELF_ACCESS]]
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         end_access [[SELF_ACCESS]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements2P1P4ubgsAA1UVvx'
}

@available(SwiftStdlib 9999, *)
public protocol P2 : ~Copyable {
  var urs: U { yielding borrow set }

// A `{ yielding borrow set }` protocol requirement builds default
// yielding_borrow/yielding_mutate thunks that dispatch to conformer-provided
// _read/_modify implementations.
// New conformers override these with the real implementations and provide
// _read/_modify thunks.

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements2P2P3ursAA1UVvy : $@yield_once_2

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements2P2P3ursAA1UVvx : $@yield_once_2    
}


@available(SwiftStdlib 6.0, *)
public protocol P3 : ~Copyable {
  var ur: U { yielding borrow }

// A `{ yielding borrow }` protocol requirement builds a default
// yielding_borrow thunks that dispatches to conformer-provided
// _read implementations.
// New conformers override these with the real implementations and provide
// _read thunks.

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements2P3P2urAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_UNCHECKED:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[SELF_UNCHECKED]]
// CHECK:         [[READER:%[^,]+]] = witness_method $Self, #P3.ur!read
// CHECK:         ([[VALUE_UNCHECKED:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[READER]]<Self>([[SELF]])
// CHECK:         [[VALUE_COPY:%[^,]+]] = copy_value [[VALUE_UNCHECKED]]
// CHECK:         [[VALUE:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[VALUE_COPY]]
// CHECK:         [[VALUE_BORROW:%[^,]+]] = begin_borrow [[VALUE]]
// CHECK:         yield [[VALUE_BORROW:%[^,]+]]
// CHECK-SAME:             resume [[BASIC_BLOCK1:bb[0-9]+]]
// CHECK-SAME:             unwind [[BASIC_BLOCK2:bb[0-9]+]]
// CHECK:       [[BASIC_BLOCK1]]:
// CHECK:         end_borrow [[VALUE_BORROW]]
// CHECK:         destroy_value [[VALUE]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:       [[BASIC_BLOCK2]]:
// CHECK:         end_borrow [[VALUE_BORROW]]
// CHECK:         destroy_value [[VALUE]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements2P3P2urAA1UVvy'
}

@frozen
public struct ImplAStored : ~Copyable & P1 {
  public var ubgs: U
// ImplAStored is a plain stored property, so it doesn't naturally need the
// legacy `!read` accessor -- it exists only as an on-demand witness for P1's
// unconditionally-required slot.  On an ABI-stable platform it's emitted
// eagerly, right before yielding_borrow, so this exhaustive check is
// stable-only.  On a non-ABI-stable platform it's deferred until right after
// the witness thunk that needs it instead; its existence there is still
// verified, just not by this exhaustive FileCheck block: the witness thunk's
// own check below requires a function_ref to it, and the -sil-verify-all RUN
// line above would fail if it had no body.

// CHECK-stable-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAStoredV4ubgsAA1UVvr : {{.*}} {
// CHECK-stable:       bb0(
// CHECK-stable-SAME:      [[SELF:%[^:]+]]
// CHECK-stable-SAME:  ):
// CHECK-stable:         [[COPY_UNCHECKED:%[^,]+]] = copy_value [[SELF]]
// CHECK-stable:         [[COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY_UNCHECKED]]
// CHECK-stable:         [[BORROW:%[^,]+]] = begin_borrow [[COPY]]
// CHECK-stable:         [[VALUE:%[^,]+]] = struct_extract [[BORROW]]
// CHECK-stable-SAME:        #ImplAStored.ubgs
// CHECK-stable:         yield [[VALUE]]
// CHECK-stable-SAME:        resume [[SUCCESS:bb[0-9]+]]
// CHECK-stable-SAME:        unwind [[FAILURE:bb[0-9]+]]
// CHECK-stable:       [[SUCCESS]]:
// CHECK-stable:         end_borrow [[BORROW]]
// CHECK-stable:         destroy_value [[COPY]]
// CHECK-stable:         return
// CHECK-stable:       [[FAILURE]]:
// CHECK-stable:         end_borrow [[BORROW]]
// CHECK-stable:         destroy_value [[COPY]]
// CHECK-stable:         unwind
// CHECK-stable-LABEL: } // end sil function '$s17read_requirements11ImplAStoredV4ubgsAA1UVvr'

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAStoredV4ubgsAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[COPY_UNCHECKED:%[^,]+]] = copy_value [[SELF]]
// CHECK:         [[COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY_UNCHECKED]]
// CHECK:         [[BORROW:%[^,]+]] = begin_borrow [[COPY]]
// CHECK:         [[VALUE:%[^,]+]] = struct_extract [[BORROW]]
// CHECK-SAME:        #ImplAStored.ubgs
// CHECK:         yield [[VALUE]]
// CHECK-SAME:        resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:        unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplAStoredV4ubgsAA1UVvy'

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAStoredVAA2P1A2aDP4ubgsAA1UVvrTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements11ImplAStoredV4ubgsAA1UVvr
// CHECK:         ([[VALUE_ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[READER]]([[SELF]])
// CHECK:         yield [[VALUE_ADDR]]
// CHECK-SAME:        resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:        unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplAStoredVAA2P1A2aDP4ubgsAA1UVvrTW'

// Annoyingly, on a non-ABI-stable platform !read is emitted in a different
// order.  Just check the signature exists here.

// CHECK-unstable-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAStoredV4ubgsAA1UVvr : {{.*}} {

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAStoredVAA2P1A2aDP4ubgsAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[READ2ER:%[^,]+]] = function_ref @$s17read_requirements11ImplAStoredV4ubgsAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE:%[^,]+]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       bb1:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         return
// CHECK:       bb2:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplAStoredVAA2P1A2aDP4ubgsAA1UVvyTW'
}

@frozen
@available(SwiftStdlib 9999, *)
public struct ImplBStored : ~Copyable & P2 {
  var dummy: ()
  public var urs: U

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplBStoredV3ursAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[COPY_UNCHECKED:%[^,]+]] = copy_value [[SELF]]
// CHECK:         [[COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY_UNCHECKED]]
// CHECK:         debug_value [[COPY]]
// CHECK:         [[BORROW:%[^,]+]] = begin_borrow [[COPY]]
// CHECK:         [[VALUE:%[^,]+]] = struct_extract [[BORROW]]
// CHECK-SAME:        #ImplBStored.urs
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplBStoredV3ursAA1UVvy'

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplBStoredVAA2P2A2aDP3ursAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements11ImplBStoredV3ursAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplBStoredVAA2P2A2aDP3ursAA1UVvyTW'
}

@frozen
@available(SwiftStdlib 6.0, *)
public struct ImplCStored : ~Copyable & P3 {
  var dummy: ()
  public var ur: U
// ImplCStored is a plain stored property, so it doesn't naturally need the
// legacy `!read` accessor -- it exists only as an on-demand witness for P3's
// unconditionally-required slot.  On an ABI-stable platform it's emitted
// eagerly, right before yielding_borrow, so this exhaustive check is
// stable-only.  On a non-ABI-stable platform it's deferred until right after
// the witness thunk that needs it instead; its existence there is still
// verified, just not by this exhaustive FileCheck block: the witness thunk's
// own check below requires a function_ref to it, and the -sil-verify-all RUN
// line above would fail if it had no body.

// CHECK-stable-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCStoredV2urAA1UVvr : {{.*}} {
// CHECK-stable:       bb0(
// CHECK-stable-SAME:      [[SELF:%[^:]+]]
// CHECK-stable-SAME:  ):
// CHECK-stable:         [[COPY_UNCHECKED:%[^,]+]] = copy_value [[SELF]]
// CHECK-stable:         [[COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY_UNCHECKED]]
// CHECK-stable:         [[BORROW:%[^,]+]] = begin_borrow [[COPY]]
// CHECK-stable:         [[VALUE:%[^,]+]] = struct_extract [[BORROW]]
// CHECK-stable-SAME:        #ImplCStored.ur
// CHECK-stable:         yield [[VALUE]]
// CHECK-stable-SAME:        resume [[SUCCESS:bb[0-9]+]]
// CHECK-stable-SAME:        unwind [[FAILURE:bb[0-9]+]]
// CHECK-stable:       [[SUCCESS]]:
// CHECK-stable:         end_borrow [[BORROW]]
// CHECK-stable:         destroy_value [[COPY]]
// CHECK-stable:         return
// CHECK-stable:       [[FAILURE]]:
// CHECK-stable:         end_borrow [[BORROW]]
// CHECK-stable:         destroy_value [[COPY]]
// CHECK-stable:         unwind
// CHECK-stable-LABEL: } // end sil function '$s17read_requirements11ImplCStoredV2urAA1UVvr'

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCStoredV2urAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[COPY_UNCHECKED:%[^,]+]] = copy_value [[SELF]]
// CHECK:         [[COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY_UNCHECKED]]
// CHECK:         [[BORROW:%[^,]+]] = begin_borrow [[COPY]]
// CHECK:         [[VALUE:%[^,]+]] = struct_extract [[BORROW]]
// CHECK-SAME:        #ImplCStored.ur
// CHECK:         yield [[VALUE]]
// CHECK-SAME:        resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:        unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplCStoredV2urAA1UVvy'

// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCStoredVAA2P3A2aDP2urAA1UVvrTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements11ImplCStoredV2urAA1UVvr
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[READER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplCStoredVAA2P3A2aDP2urAA1UVvrTW'

// On a non-ABI-stable platform !read is deferred to exactly this point --
// confirm at least its signature, since the exhaustive check above is
// stable-only.
// CHECK-unstable-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCStoredV2urAA1UVvr : {{.*}} {
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCStoredVAA2P3A2aDP2urAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements11ImplCStoredV2urAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplCStoredVAA2P3A2aDP2urAA1UVvyTW'
}

@frozen
public struct ImplALegacyCoroutineAccessors : ~Copyable & P1 {
  var _i: U
  public var ubgs: U {
    _read {
      yield _i
    }
    _modify {
      yield &_i
    }
  }
// With the CoroutineAccessors feature enabled, `_read`/`_modify` use the same
// yield_once_2 ABI as `yielding borrow`/`yielding mutate`, so this type emits
// the same accessors as ImplACoroutineAccessors above (modulo the mangled type
// name), which verify the bodies in full.  Here we confirm the accessor set and
// forwarding: the yield_once_2 vy/vx are the primary implementations and, because
// this type predates the feature, the additive yield_once vr/vM forward to them.
// CHECK-LABEL: sil{{.*}} @$s17read_requirements29ImplALegacyCoroutineAccessorsV4ubgsAA1UVvy : $@yield_once_2 @convention
// CHECK-LABEL: sil{{.*}} @$s17read_requirements29ImplALegacyCoroutineAccessorsV4ubgsAA1UVvx : $@yield_once_2 @convention
// CHECK-LABEL: sil{{.*}} @$s17read_requirements29ImplALegacyCoroutineAccessorsV4ubgsAA1UVvr : $@yield_once @convention
// CHECK:         function_ref @$s17read_requirements29ImplALegacyCoroutineAccessorsV4ubgsAA1UVvy
// CHECK-LABEL: sil{{.*}} @$s17read_requirements29ImplALegacyCoroutineAccessorsV4ubgsAA1UVvM : $@yield_once @convention
// CHECK:         function_ref @$s17read_requirements29ImplALegacyCoroutineAccessorsV4ubgsAA1UVvx
}

@frozen
@available(SwiftStdlib 9999, *)
public struct ImplBLegacyCoroutineAccessors : ~Copyable & P2 {
  var _i: U
  public var urs: U {
    _read {
      yield _i
    }
    _modify {
      yield &_i
    }
  }
// ImplB is introduced at the feature's own availability, so on its own its
// concrete accessor set would skip the legacy ABI (only yield_once_2 vy/vx).
// But it conforms to P2, whose `!read`/`!modify` witness slots are frozen and
// required unconditionally with no default implementation (see P2's default
// witness table above) -- so satisfying the conformance forces the additive
// yield_once vr/vM here too, forwarding to vy/vx just like ImplA's.
// CHECK-LABEL: sil{{.*}} @$s17read_requirements29ImplBLegacyCoroutineAccessorsV3ursAA1UVvy : $@yield_once_2 @convention
// CHECK-LABEL: sil{{.*}} @$s17read_requirements29ImplBLegacyCoroutineAccessorsV3ursAA1UVvx : $@yield_once_2 @convention
// CHECK-LABEL: sil{{.*}} @$s17read_requirements29ImplBLegacyCoroutineAccessorsV3ursAA1UVvr : $@yield_once @convention
// CHECK:         function_ref @$s17read_requirements29ImplBLegacyCoroutineAccessorsV3ursAA1UVvy
// CHECK-LABEL: sil{{.*}} @$s17read_requirements29ImplBLegacyCoroutineAccessorsV3ursAA1UVvM : $@yield_once @convention
// CHECK:         function_ref @$s17read_requirements29ImplBLegacyCoroutineAccessorsV3ursAA1UVvx
}

@frozen
@available(SwiftStdlib 6.0, *)
public struct ImplCLegacyCoroutineAccessors : ~Copyable & P3 {
  var _i: U
  public var ur: U {
    _read {
      yield _i
    }
  }
// With the CoroutineAccessors feature enabled, `_read` uses the same yield_once_2
// ABI as `yielding borrow`, so this type emits the same accessors as
// ImplCCoroutineAccessors above (modulo the mangled type name), which verify the
// body in full.  Here we confirm the accessor set and forwarding: the
// yield_once_2 vy is the primary implementation and, because this type predates
// the feature, the additive yield_once vr forwards to it.
// CHECK-LABEL: sil{{.*}} @$s17read_requirements29ImplCLegacyCoroutineAccessorsV2urAA1UVvy : $@yield_once_2 @convention
// CHECK-LABEL: sil{{.*}} @$s17read_requirements29ImplCLegacyCoroutineAccessorsV2urAA1UVvr : $@yield_once @convention
// CHECK:         function_ref @$s17read_requirements29ImplCLegacyCoroutineAccessorsV2urAA1UVvy
}

struct ImplACoroutineAccessors : ~Copyable & P1 {
  var _i: U
  var ubgs: U {
    yielding borrow {
      yield _i
    }
    yielding mutate {
      yield &_i
    }
  }
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplACoroutineAccessorsV4ubgsAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK:           [[SELF:%[^:]+]]
// CHECK:       ):
// CHECK:         [[COPY_UNCHECKED:%[^,]+]] = copy_value [[SELF]]
// CHECK:         [[COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY_UNCHECKED]]
// CHECK:         [[BORROW:%[^,]+]] = begin_borrow [[COPY]]
// CHECK:         [[VALUE:%[^,]+]] = struct_extract [[BORROW]]
// CHECK:             #ImplACoroutineAccessors._i
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements23ImplACoroutineAccessorsV4ubgsAA1UVvy'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplACoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvrTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements23ImplACoroutineAccessorsV4ubgsAA1UVvr
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[READER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:        resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:        unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements23ImplACoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvrTW'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplACoroutineAccessorsV4ubgsAA1UVvr : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[COPY_UNCHECKED:%[^,]+]] = copy_value [[SELF]]
// CHECK:         [[COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY_UNCHECKED]]
// CHECK:         [[BORROW:%[^,]+]] = begin_borrow [[COPY]]
// CHECK:         [[READ2ER:%[^,]+]] = function_ref @$s17read_requirements23ImplACoroutineAccessorsV4ubgsAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[BORROW]])
// CHECK:         [[VALUE_COPY_UNCHECKED:%[^,]+]] = copy_value [[VALUE]]
// CHECK:         [[VALUE_COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[VALUE_COPY_UNCHECKED]]
// CHECK:         [[VALUE_BORROW:%[^,]+]] = begin_borrow [[VALUE_COPY]]
// CHECK:         yield [[VALUE_BORROW]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_borrow [[VALUE_BORROW]]
// CHECK:         destroy_value [[VALUE_COPY]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_borrow [[VALUE_BORROW]]
// CHECK:         destroy_value [[VALUE_COPY]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements23ImplACoroutineAccessorsV4ubgsAA1UVvr'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplACoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[READ2ER:%[^,]+]] = function_ref @$s17read_requirements23ImplACoroutineAccessorsV4ubgsAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements23ImplACoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvyTW'
}

@frozen
@available(SwiftStdlib 9999, *)
public struct ImplBCoroutineAccessors : ~Copyable & P2 {
  var _i: U
  public var urs: U {
    yielding borrow {
      yield _i
    }
    yielding mutate {
      yield &_i
    }
  }
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplBCoroutineAccessorsV3ursAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[COPY_UNCHECKED:%[^,]+]] = copy_value [[SELF]]
// CHECK:         [[COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY_UNCHECKED]]
// CHECK:         [[BORROW:%[^,]+]] = begin_borrow [[COPY]]
// CHECK:         [[VALUE:%[^,]+]] = struct_extract [[BORROW]] : $ImplBCoroutineAccessors
// CHECK:             #ImplBCoroutineAccessors._i
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements23ImplBCoroutineAccessorsV3ursAA1UVvy'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplBCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[READ2ER:%[^,]+]] = function_ref @$s17read_requirements23ImplBCoroutineAccessorsV3ursAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements23ImplBCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvyTW'
}

@frozen
@available(SwiftStdlib 6.0, *)
public struct ImplCCoroutineAccessors : ~Copyable & P3 {
  var _i: U
  public var ur: U {
    yielding borrow {
      yield _i
    }
  }
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplCCoroutineAccessorsV2urAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[COPY_UNCHECKED:%[^,]+]] = copy_value [[SELF]]
// CHECK:         [[COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY_UNCHECKED]]
// CHECK:         [[BORROW:%[^,]+]] = begin_borrow [[COPY]]
// CHECK:         [[VALUE:%[^,]+]] = struct_extract [[BORROW]]
// CHECK-SAME:        #ImplCCoroutineAccessors._i
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[COPY]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements23ImplCCoroutineAccessorsV2urAA1UVvy'
// ImplCCoroutineAccessors writes `ur` using `yielding borrow`, so `!yield`
// (vy) is the primary implementation and this `!read` (vr) is only an
// on-demand witness for P3's unconditionally-required legacy slot.  On an
// ABI-stable platform it's emitted eagerly, right after vy, so this
// exhaustive check is stable-only.  On a non-ABI-stable platform it's
// deferred until right after the witness thunk that needs it instead; its
// existence there is still verified, just not by this exhaustive FileCheck
// block: the witness thunk's own check below requires a function_ref to it,
// and the -sil-verify-all RUN line above would fail if it had no body.
// CHECK-stable-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplCCoroutineAccessorsV2urAA1UVvr : {{.*}} {
// CHECK-stable:       bb0(
// CHECK-stable-SAME:      [[SELF:%[^:]+]]
// CHECK-stable-SAME:  ):
// CHECK-stable:         [[COPY_UNCHECKED:%[^,]+]] = copy_value [[SELF:%[^,]+]]
// CHECK-stable:         [[COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY_UNCHECKED]]
// CHECK-stable:         [[BORROW:%[^,]+]] = begin_borrow [[COPY]]
// CHECK-stable:         [[READ2ER:%[^,]+]] = function_ref @$s17read_requirements23ImplCCoroutineAccessorsV2urAA1UVvy
// CHECK-stable:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[BORROW]])
// CHECK-stable:         [[VALUE_COPY_UNCHECKED:%[^,]+]] = copy_value [[VALUE:%[^,]+]]
// CHECK-stable:         [[VALUE_COPY:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[VALUE_COPY_UNCHECKED]]
// CHECK-stable:         [[VALUE_BORROW:%[^,]+]] = begin_borrow [[VALUE_COPY]]
// CHECK-stable:         yield [[VALUE_BORROW]]
// CHECK-stable-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-stable-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK-stable:       [[SUCCESS]]:
// CHECK-stable:         end_borrow [[VALUE_BORROW]]
// CHECK-stable:         destroy_value [[VALUE_COPY]]
// CHECK-stable:         end_apply [[TOKEN]]
// CHECK-stable:         dealloc_stack [[ALLOCATION]]
// CHECK-stable:         end_borrow [[BORROW]]
// CHECK-stable:         destroy_value [[COPY]]
// CHECK-stable:         return
// CHECK-stable:       [[FAILURE]]:
// CHECK-stable:         end_borrow [[VALUE_BORROW]]
// CHECK-stable:         destroy_value [[VALUE_COPY]]
// CHECK-stable:         end_apply [[TOKEN]]
// CHECK-stable:         dealloc_stack [[ALLOCATION]]
// CHECK-stable:         end_borrow [[BORROW]]
// CHECK-stable:         destroy_value [[COPY]]
// CHECK-stable:         unwind
// CHECK-stable-LABEL: } // end sil function '$s17read_requirements23ImplCCoroutineAccessorsV2urAA1UVvr'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplCCoroutineAccessorsVAA2P3A2aDP2urAA1UVvrTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements23ImplCCoroutineAccessorsV2urAA1UVvr
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[READER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:        resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:        unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements23ImplCCoroutineAccessorsVAA2P3A2aDP2urAA1UVvrTW'
// On a non-ABI-stable platform !read is deferred to exactly this point --
// confirm at least its signature, since the exhaustive check above is
// stable-only.
// CHECK-unstable-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplCCoroutineAccessorsV2urAA1UVvr : {{.*}} {
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements23ImplCCoroutineAccessorsVAA2P3A2aDP2urAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[READ2ER:%[^,]+]] = function_ref @$s17read_requirements23ImplCCoroutineAccessorsV2urAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         end_borrow [[SELF]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements23ImplCCoroutineAccessorsVAA2P3A2aDP2urAA1UVvyTW'
}

@frozen
public struct ImplAGetSet : P1 {
  var _i: U {
    get { return U() }
    set {}
  }
  public var ubgs: U {
    get {
      return _i
    }
    set {
      _i = newValue
    }
  }
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAGetSetV4ubgsAA1UVvg : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[_I_GETTER:%[^,]+]] = function_ref @$s17read_requirements11ImplAGetSetV2_iAA1UVvg
// CHECK:         [[VALUE:%[^,]+]] = apply [[_I_GETTER]]([[SELF]])
// CHECK:         return [[VALUE]]
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplAGetSetV4ubgsAA1UVvg'
// ImplAGetSet implements `ubgs` with a plain get/set, so it doesn't naturally
// need the legacy `!read` accessor -- it exists only as an on-demand witness
// for P1's unconditionally-required slot.  On an ABI-stable platform it's
// emitted eagerly, right after the getter, so this exhaustive check is
// stable-only.  On a non-ABI-stable platform it's deferred until right after
// the witness thunk that needs it instead; its existence there is still
// verified, just not by this exhaustive FileCheck block: the witness thunk's
// own check below requires a function_ref to it, and the -sil-verify-all RUN
// line above would fail if it had no body.
// CHECK-stable-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAGetSetV4ubgsAA1UVvr : {{.*}} {
// CHECK-stable:       bb0(
// CHECK-stable-SAME:      [[SELF:%[^:]+]] :
// CHECK-stable-SAME:  ):
// CHECK-stable:         [[GETTER:%[^,]+]] = function_ref @$s17read_requirements11ImplAGetSetV4ubgsAA1UVvg
// CHECK-stable:         [[VALUE:%[^,]+]] = apply [[GETTER]]([[SELF]])
// CHECK-stable:         [[BORROW:%[^,]+]] = begin_borrow [[VALUE]]
// CHECK-stable:         yield [[BORROW]]
// CHECK-stable-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-stable-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK-stable:       [[SUCCESS]]:
// CHECK-stable:         end_borrow [[BORROW]]
// CHECK-stable:         destroy_value [[VALUE]]
// CHECK-stable:         return
// CHECK-stable:       [[FAILURE]]:
// CHECK-stable:         end_borrow [[BORROW]]
// CHECK-stable:         destroy_value [[VALUE]]
// CHECK-stable:         unwind
// CHECK-stable-LABEL: } // end sil function '$s17read_requirements11ImplAGetSetV4ubgsAA1UVvr'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAGetSetV4ubgsAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[GETTER:%[^,]+]] = function_ref @$s17read_requirements11ImplAGetSetV4ubgsAA1UVvg
// CHECK:         [[VALUE:%[^,]+]] = apply [[GETTER]]([[SELF]])
// CHECK:         [[BORROW:%[^,]+]] = begin_borrow [[VALUE]]
// CHECK:         yield [[BORROW]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[VALUE]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[VALUE]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplAGetSetV4ubgsAA1UVvy'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAGetSetVAA2P1A2aDP4ubgsAA1UVvrTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load [trivial] [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements11ImplAGetSetV4ubgsAA1UVvr
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[READER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplAGetSetVAA2P1A2aDP4ubgsAA1UVvrTW'
// On a non-ABI-stable platform !read is deferred to exactly this point --
// confirm at least its signature, since the exhaustive check above is
// stable-only.
// CHECK-unstable-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAGetSetV4ubgsAA1UVvr : {{.*}} {
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplAGetSetVAA2P1A2aDP4ubgsAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load [trivial] [[SELF_ADDR]]
// CHECK:         [[READ2ER:%[^,]+]] = function_ref @$s17read_requirements11ImplAGetSetV4ubgsAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplAGetSetVAA2P1A2aDP4ubgsAA1UVvyTW'
}

@frozen
@available(SwiftStdlib 9999, *)
public struct ImplBGetSet : P2 {
  var _i: U {
    get { return U() }
    set {}
  }
  public var urs: U {
    get {
      return _i
    }
    set {
      _i = newValue
    }
  }
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplBGetSetV3ursAA1UVvg : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[_I_GETTER:%[^,]+]] = function_ref @$s17read_requirements11ImplBGetSetV2_iAA1UVvg
// CHECK:         [[VALUE:%[^,]+]] = apply [[_I_GETTER]]([[SELF]])
// CHECK:         return [[VALUE]]
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplBGetSetV3ursAA1UVvg'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplBGetSetV3ursAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[GETTER:%[^,]+]] = function_ref @$s17read_requirements11ImplBGetSetV3ursAA1UVvg
// CHECK:         [[VALUE:%[^,]+]] = apply [[GETTER]]([[SELF]])
// CHECK:         [[BORROW:%[^,]+]] = begin_borrow [[VALUE]]
// CHECK:         yield [[BORROW]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[VALUE]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[VALUE]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplBGetSetV3ursAA1UVvy'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplBGetSetVAA2P2A2aDP3ursAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load [trivial] [[SELF_ADDR]]
// CHECK:         [[READ2ER:%[^,]+]] = function_ref @$s17read_requirements11ImplBGetSetV3ursAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplBGetSetVAA2P2A2aDP3ursAA1UVvyTW'
}

@frozen
@available(SwiftStdlib 6.0, *)
public struct ImplCGetSet : P3 {
  var _i: U {
    get { return U() }
    set {}
  }
  public var ur: U {
    get {
      return _i
    }
  }
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCGetSetV2urAA1UVvg : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[_I_GETTER:%[^,]+]] = function_ref @$s17read_requirements11ImplCGetSetV2_iAA1UVvg
// CHECK:         [[VALUE:%[^,]+]] = apply [[_I_GETTER]]([[SELF]])
// CHECK:         return [[VALUE]]
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplCGetSetV2urAA1UVvg'
// ImplCGetSet implements `ur` with a plain get, so it doesn't naturally need
// the legacy `!read` accessor -- it exists only as an on-demand witness for
// P3's unconditionally-required slot.  On an ABI-stable platform it's
// emitted eagerly, right after the getter, so this exhaustive check is
// stable-only.  On a non-ABI-stable platform it's deferred until right after
// the witness thunk that needs it instead; its existence there is still
// verified, just not by this exhaustive FileCheck block: the witness thunk's
// own check below requires a function_ref to it, and the -sil-verify-all RUN
// line above would fail if it had no body.
// CHECK-stable-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCGetSetV2urAA1UVvr : {{.*}} {
// CHECK-stable:       bb0(
// CHECK-stable-SAME:      [[SELF:%[^:]+]] :
// CHECK-stable-SAME:  ):
// CHECK-stable:         [[GETTER:%[^,]+]] = function_ref @$s17read_requirements11ImplCGetSetV2urAA1UVvg
// CHECK-stable:         [[VALUE:%[^,]+]] = apply [[GETTER]]([[SELF]])
// CHECK-stable:         [[BORROW:%[^,]+]] = begin_borrow [[VALUE]]
// CHECK-stable:         yield [[BORROW]]
// CHECK-stable-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-stable-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK-stable:       [[SUCCESS]]:
// CHECK-stable:         end_borrow [[BORROW]]
// CHECK-stable:         destroy_value [[VALUE]]
// CHECK-stable:         return
// CHECK-stable:       [[FAILURE]]:
// CHECK-stable:         end_borrow [[BORROW]]
// CHECK-stable:         destroy_value [[VALUE]]
// CHECK-stable:         unwind
// CHECK-stable-LABEL: } // end sil function '$s17read_requirements11ImplCGetSetV2urAA1UVvr'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCGetSetV2urAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[GETTER:%[^,]+]] = function_ref @$s17read_requirements11ImplCGetSetV2urAA1UVvg
// CHECK:         [[VALUE:%[^,]+]] = apply [[GETTER]]([[SELF]])
// CHECK:         [[BORROW:%[^,]+]] = begin_borrow [[VALUE]]
// CHECK:         yield [[BORROW]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[VALUE]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[VALUE]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplCGetSetV2urAA1UVvy'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCGetSetVAA2P3A2aDP2urAA1UVvrTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load [trivial] [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements11ImplCGetSetV2urAA1UVvr
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[READER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplCGetSetVAA2P3A2aDP2urAA1UVvrTW'
// On a non-ABI-stable platform !read is deferred to exactly this point --
// confirm at least its signature, since the exhaustive check above is
// stable-only.
// CHECK-unstable-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCGetSetV2urAA1UVvr : {{.*}} {
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements11ImplCGetSetVAA2P3A2aDP2urAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load [trivial] [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements11ImplCGetSetV2urAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements11ImplCGetSetVAA2P3A2aDP2urAA1UVvyTW'
}

@frozen
public struct ImplAUnsafeAddressors : P1 {
  var iAddr: UnsafePointer<U>
  var iMutableAddr: UnsafeMutablePointer<U> {
    .init(mutating: iAddr)
  }
  public var ubgs: U {
    unsafeAddress {
      return iAddr
    }
    unsafeMutableAddress {
      return iMutableAddr
    }
  }
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvlu : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[UNSAFE_POINTER:%[^,]+]] = struct_extract [[SELF]] : $ImplAUnsafeAddressors
// CHECK:             #ImplAUnsafeAddressors.iAddr
// CHECK:         return [[UNSAFE_POINTER]]
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvlu'
// ImplAUnsafeAddressors implements `ubgs` with unsafeAddress/unsafeMutable-
// Address, so it doesn't naturally need the legacy `!read` accessor -- it
// exists only as an on-demand witness for P1's unconditionally-required
// slot.  On an ABI-stable platform it's emitted eagerly, right after the
// unsafe addressor, so this exhaustive check is stable-only.  On a
// non-ABI-stable platform it's deferred until right after the witness thunk
// that needs it instead; its existence there is still verified, just not by
// this exhaustive FileCheck block: the witness thunk's own check below
// requires a function_ref to it, and the -sil-verify-all RUN line above
// would fail if it had no body.
// CHECK-stable-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvr : {{.*}} {
// CHECK-stable:       bb0(
// CHECK-stable-SAME:      [[SELF:%[^:]+]] :
// CHECK-stable-SAME:  ):
// CHECK-stable:         [[UNSAFE_ADDRESSOR:%[^,]+]] = function_ref @$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvlu
// CHECK-stable:         [[UNSAFE_POINTER:%[^,]+]] = apply [[UNSAFE_ADDRESSOR]]([[SELF]])
// CHECK-stable:         [[RAW_POINTER:%[^,]+]] = struct_extract [[UNSAFE_POINTER]] : $UnsafePointer<U>, #UnsafePointer._rawValue
// CHECK-stable:         [[ADDR:%[^,]+]] = pointer_to_address [[RAW_POINTER]]
// CHECK-stable:         [[MD:%.*]] = mark_dependence [unresolved] [[ADDR]] : $*U on [[SELF]]
// CHECK-stable:         [[ACCESS_UNCHECKED:%[^,]+]] = begin_access [read] [unsafe] [[MD]]
// CHECK-stable:         [[ACCESS:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS_UNCHECKED]]
// CHECK-stable:         [[VALUE:%[^,]+]] = load [copy] [[ACCESS]]
// CHECK-stable:         yield [[VALUE]]
// CHECK-stable-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-stable-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK-stable:       [[SUCCESS]]:
// CHECK-stable:         destroy_value [[VALUE]]
// CHECK-stable:         end_access [[ACCESS_UNCHECKED]]
// CHECK-stable:         return
// CHECK-stable:       [[FAILURE]]:
// CHECK-stable:         destroy_value [[VALUE]]
// CHECK-stable:         end_access [[ACCESS_UNCHECKED]]
// CHECK-stable:         unwind
// CHECK-stable-LABEL: } // end sil function '$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvr'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[UNSAFE_ADDRESSOR:%[^,]+]] = function_ref @$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvlu
// CHECK:         [[UNSAFE_POINTER:%[^,]+]] = apply [[UNSAFE_ADDRESSOR]]([[SELF]])
// CHECK:         [[RAW_POINTER:%[^,]+]] = struct_extract [[UNSAFE_POINTER]] : $UnsafePointer<U>, #UnsafePointer._rawValue
// CHECK:         [[ADDR:%[^,]+]] = pointer_to_address [[RAW_POINTER]]
// CHECK:         [[MD:%.*]] = mark_dependence [unresolved] [[ADDR]] : $*U on [[SELF]]
// CHECK:         [[ACCESS_UNCHECKED:%[^,]+]] = begin_access [read] [unsafe] [[MD]]
// CHECK:         [[ACCESS:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS_UNCHECKED]]
// CHECK:         [[VALUE:%[^,]+]] = load [copy] [[ACCESS]]
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         destroy_value [[VALUE]]
// CHECK:         end_access [[ACCESS_UNCHECKED]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         destroy_value [[VALUE]]
// CHECK:         end_access [[ACCESS_UNCHECKED]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvy'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplAUnsafeAddressorsVAA2P1A2aDP4ubgsAA1UVvrTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR]]
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load [trivial] [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvr
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[READER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplAUnsafeAddressorsVAA2P1A2aDP4ubgsAA1UVvrTW'
// On a non-ABI-stable platform !read is deferred to exactly this point --
// confirm at least its signature, since the exhaustive check above is
// stable-only.
// CHECK-unstable-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvr : {{.*}} {
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplAUnsafeAddressorsVAA2P1A2aDP4ubgsAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load [trivial] [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements21ImplAUnsafeAddressorsV4ubgsAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplAUnsafeAddressorsVAA2P1A2aDP4ubgsAA1UVvyTW'
}

@frozen
@available(SwiftStdlib 9999, *)
public struct ImplBUnsafeAddressors : P2 {
  var iAddr: UnsafePointer<U>
  var iMutableAddr: UnsafeMutablePointer<U> {
    .init(mutating: iAddr)
  }
  public var urs: U {
    unsafeAddress {
      return iAddr
    }
    unsafeMutableAddress {
      return iMutableAddr
    }
  }
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplBUnsafeAddressorsV3ursAA1UVvlu : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[UNSAFE_POINTER:%[^,]+]] = struct_extract [[SELF]]
// CHECK:             #ImplBUnsafeAddressors.iAddr
// CHECK:         return [[UNSAFE_POINTER]]
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplBUnsafeAddressorsV3ursAA1UVvlu'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplBUnsafeAddressorsV3ursAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[UNSAFE_ADDRESSOR:%[^,]+]] = function_ref @$s17read_requirements21ImplBUnsafeAddressorsV3ursAA1UVvlu
// CHECK:         [[UNSAFE_POINTER:%[^,]+]] = apply [[UNSAFE_ADDRESSOR]]([[SELF]])
// CHECK:         [[RAW_POINTER:%[^,]+]] = struct_extract [[UNSAFE_POINTER]]
// CHECK:             #UnsafePointer._rawValue
// CHECK:         [[ADDR:%[^,]+]] = pointer_to_address [[RAW_POINTER]]
// CHECK:         [[MD:%.*]] = mark_dependence [unresolved] [[ADDR]] : $*U on [[SELF]]
// CHECK:         [[ACCESS_UNCHECKED:%[^,]+]] = begin_access [read] [unsafe] [[MD]]
// CHECK:         [[ACCESS:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS_UNCHECKED]]
// CHECK:         [[VALUE:%[^,]+]] = load [copy] [[ACCESS]]
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         destroy_value [[VALUE]]
// CHECK:         end_access [[ACCESS_UNCHECKED]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         destroy_value [[VALUE]]
// CHECK:         end_access [[ACCESS_UNCHECKED]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplBUnsafeAddressorsV3ursAA1UVvy'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplBUnsafeAddressorsVAA2P2A2aDP3ursAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load [trivial] [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements21ImplBUnsafeAddressorsV3ursAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplBUnsafeAddressorsVAA2P2A2aDP3ursAA1UVvyTW'
}

@frozen
@available(SwiftStdlib 6.0, *)
public struct ImplCUnsafeAddressors : P3 {
  var iAddr: UnsafePointer<U>
  var iMutableAddr: UnsafeMutablePointer<U> {
    .init(mutating: iAddr)
  }
  public var ur: U {
    unsafeAddress {
      return iAddr
    }
    unsafeMutableAddress {
      return iMutableAddr
    }
  }
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvlu : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[UNSAFE_POINTER:%[^,]+]] = struct_extract [[SELF]]
// CHECK:             #ImplCUnsafeAddressors.iAddr
// CHECK:         return [[UNSAFE_POINTER]]
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvlu'
// ImplCUnsafeAddressors implements `ur` with unsafeAddress/unsafeMutable-
// Address, so it doesn't naturally need the legacy `!read` accessor -- it
// exists only as an on-demand witness for P3's unconditionally-required
// slot.  On an ABI-stable platform it's emitted eagerly, right after the
// unsafe addressor, so this exhaustive check is stable-only.  On a
// non-ABI-stable platform it's deferred until right after the witness thunk
// that needs it instead; its existence there is still verified, just not by
// this exhaustive FileCheck block: the witness thunk's own check below
// requires a function_ref to it, and the -sil-verify-all RUN line above
// would fail if it had no body.
// CHECK-stable-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvr : {{.*}} {
// CHECK-stable:       bb0(
// CHECK-stable-SAME:      [[SELF:%[^:]+]] :
// CHECK-stable-SAME:  ):
// CHECK-stable:         [[UNSAFE_ADDRESSOR:%[^,]+]] = function_ref @$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvlu
// CHECK-stable:         [[UNSAFE_POINTER:%[^,]+]] = apply [[UNSAFE_ADDRESSOR]]([[SELF]])
// CHECK-stable:         [[RAW_POINTER:%[^,]+]] = struct_extract [[UNSAFE_POINTER]]
// CHECK-stable:             #UnsafePointer._rawValue
// CHECK-stable:         [[ADDR:%[^,]+]] = pointer_to_address [[RAW_POINTER]]
// CHECK-stable:         [[MD:%.*]] = mark_dependence [unresolved] [[ADDR]] : $*U on [[SELF]]
// CHECK-stable:         [[ACCESS_UNCHECKED:%[^,]+]] = begin_access [read] [unsafe] [[MD]]
// CHECK-stable:         [[ACCESS:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS_UNCHECKED]]
// CHECK-stable:         [[VALUE:%[^,]+]] = load [copy] [[ACCESS]]
// CHECK-stable:         yield [[VALUE]]
// CHECK-stable-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-stable-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK-stable:       [[SUCCESS]]:
// CHECK-stable:         destroy_value [[VALUE]]
// CHECK-stable:         end_access [[ACCESS_UNCHECKED]]
// CHECK-stable:         return
// CHECK-stable:       [[FAILURE]]:
// CHECK-stable:         destroy_value [[VALUE]]
// CHECK-stable:         end_access [[ACCESS_UNCHECKED]]
// CHECK-stable:         unwind
// CHECK-stable-LABEL: } // end sil function '$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvr'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvy : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[UNSAFE_ADDRESSOR:%[^,]+]] = function_ref @$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvlu
// CHECK:         [[UNSAFE_POINTER:%[^,]+]] = apply [[UNSAFE_ADDRESSOR]]([[SELF]])
// CHECK:         [[RAW_POINTER:%[^,]+]] = struct_extract [[UNSAFE_POINTER]]
// CHECK:             #UnsafePointer._rawValue
// CHECK:         [[ADDR:%[^,]+]] = pointer_to_address [[RAW_POINTER]]
// CHECK:         [[MD:%.*]] = mark_dependence [unresolved] [[ADDR]] : $*U on [[SELF]]
// CHECK:         [[ACCESS_UNCHECKED:%[^,]+]] = begin_access [read] [unsafe] [[MD]]
// CHECK:         [[ACCESS:%[^,]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS_UNCHECKED]]
// CHECK:         [[VALUE:%[^,]+]] = load [copy] [[ACCESS]]
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         destroy_value [[VALUE]]
// CHECK:         end_access [[ACCESS_UNCHECKED]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         destroy_value [[VALUE]]
// CHECK:         end_access [[ACCESS_UNCHECKED]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvy'
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplCUnsafeAddressorsVAA2P3A2aDP2urAA1UVvrTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load [trivial] [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvr
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[READER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplCUnsafeAddressorsVAA2P3A2aDP2urAA1UVvrTW'
// On a non-ABI-stable platform !read is deferred to exactly this point --
// confirm at least its signature, since the exhaustive check above is
// stable-only.
// CHECK-unstable-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvr : {{.*}} {
// CHECK-LABEL: sil{{.*}} [ossa] @$s17read_requirements21ImplCUnsafeAddressorsVAA2P3A2aDP2urAA1UVvyTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^:]+]] :
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load [trivial] [[SELF_ADDR]]
// CHECK:         [[READER:%[^,]+]] = function_ref @$s17read_requirements21ImplCUnsafeAddressorsV2urAA1UVvy
// CHECK:         ([[VALUE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READ2ER]]([[SELF]])
// CHECK:         yield [[VALUE]]
// CHECK-SAME:             resume [[SUCCESS:bb[0-9]+]]
// CHECK-SAME:             unwind [[FAILURE:bb[0-9]+]]
// CHECK:       [[SUCCESS]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         return
// CHECK:       [[FAILURE]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s17read_requirements21ImplCUnsafeAddressorsVAA2P3A2aDP2urAA1UVvyTW'
}
// CHECK-LABEL: sil_witness_table{{.*}} ImplAStored: P1 module read_requirements {
// CHECK-NEXT:    method #P1.ubgs!read
// CHECK-SAME:      : @$s17read_requirements11ImplAStoredVAA2P1A2aDP4ubgsAA1UVvrTW
// CHECK-NEXT:    method #P1.ubgs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements11ImplAStoredVAA2P1A2aDP4ubgsAA1UVvyTW
// CHECK-NEXT:    method #P1.ubgs!setter
// CHECK-SAME:      : @$s17read_requirements11ImplAStoredVAA2P1A2aDP4ubgsAA1UVvsTW
// CHECK-NEXT:    method #P1.ubgs!modify
// CHECK-SAME:      : @$s17read_requirements11ImplAStoredVAA2P1A2aDP4ubgsAA1UVvMTW
// CHECK-NEXT:    method #P1.ubgs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements11ImplAStoredVAA2P1A2aDP4ubgsAA1UVvxTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplBStored: P2 module read_requirements {
// CHECK-NEXT:    method #P2.urs!read
// CHECK-SAME:      : @$s17read_requirements11ImplBStoredVAA2P2A2aDP3ursAA1UVvrTW
// CHECK-NEXT:    method #P2.urs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements11ImplBStoredVAA2P2A2aDP3ursAA1UVvyTW
// CHECK-NEXT:    method #P2.urs!setter
// CHECK-SAME:        : @$s17read_requirements11ImplBStoredVAA2P2A2aDP3ursAA1UVvsTW
// CHECK-NEXT:    method #P2.urs!modify
// CHECK-SAME:      : @$s17read_requirements11ImplBStoredVAA2P2A2aDP3ursAA1UVvMTW
// CHECK-NEXT:    method #P2.urs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements11ImplBStoredVAA2P2A2aDP3ursAA1UVvxTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplCStored: P3 module read_requirements {
// CHECK-NEXT:    method #P3.ur!read
// CHECK-SAME:        : @$s17read_requirements11ImplCStoredVAA2P3A2aDP2urAA1UVvrTW
// CHECK-NEXT:    method #P3.ur!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements11ImplCStoredVAA2P3A2aDP2urAA1UVvyTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplALegacyCoroutineAccessors: P1 module read_requirements {
// CHECK-NEXT:    method #P1.ubgs!read
// CHECK-SAME:        : @$s17read_requirements29ImplALegacyCoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvrTW
// CHECK-NEXT:    method #P1.ubgs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements29ImplALegacyCoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvyTW
// CHECK-NEXT:    method #P1.ubgs!setter
// CHECK-SAME:        : @$s17read_requirements29ImplALegacyCoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvsTW
// CHECK-NEXT:    method #P1.ubgs!modify
// CHECK-SAME:        : @$s17read_requirements29ImplALegacyCoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvMTW
// CHECK-NEXT:    method #P1.ubgs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements29ImplALegacyCoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvxTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplBLegacyCoroutineAccessors: P2 module read_requirements {
// CHECK-NEXT:    method #P2.urs!read
// CHECK-SAME:        : @$s17read_requirements29ImplBLegacyCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvrTW
// CHECK-NEXT:    method #P2.urs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements29ImplBLegacyCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvyTW
// CHECK-NEXT:    method #P2.urs!setter
// CHECK-SAME:        : @$s17read_requirements29ImplBLegacyCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvsTW
// CHECK-NEXT:    method #P2.urs!modify
// CHECK-SAME:        : @$s17read_requirements29ImplBLegacyCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvMTW
// CHECK-NEXT:    method #P2.urs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements29ImplBLegacyCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvxTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplCLegacyCoroutineAccessors: P3 module read_requirements {
// CHECK-NEXT:    method #P3.ur!read
// CHECK-SAME:        : @$s17read_requirements29ImplCLegacyCoroutineAccessorsVAA2P3A2aDP2urAA1UVvrTW
// CHECK-NEXT:    method #P3.ur!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements29ImplCLegacyCoroutineAccessorsVAA2P3A2aDP2urAA1UVvyTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplACoroutineAccessors: P1 module read_requirements {
// CHECK-NEXT:    method #P1.ubgs!read
// CHECK-SAME:        : @$s17read_requirements23ImplACoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvrTW
// CHECK-NEXT:    method #P1.ubgs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements23ImplACoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvyTW
// CHECK-NEXT:    method #P1.ubgs!setter
// CHECK-SAME:        : @$s17read_requirements23ImplACoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvsTW
// CHECK-NEXT:    method #P1.ubgs!modify
// CHECK-SAME:        : @$s17read_requirements23ImplACoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvMTW
// CHECK-NEXT:    method #P1.ubgs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements23ImplACoroutineAccessorsVAA2P1A2aDP4ubgsAA1UVvxTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplBCoroutineAccessors: P2 module read_requirements {
// CHECK-NEXT:    method #P2.urs!read
// CHECK-SAME:        : @$s17read_requirements23ImplBCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvrTW
// CHECK-NEXT:    method #P2.urs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements23ImplBCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvyTW
// CHECK-NEXT:    method #P2.urs!setter
// CHECK-SAME:        : @$s17read_requirements23ImplBCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvsTW
// CHECK-NEXT:    method #P2.urs!modify
// CHECK-SAME:        : @$s17read_requirements23ImplBCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvMTW
// CHECK-NEXT:    method #P2.urs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements23ImplBCoroutineAccessorsVAA2P2A2aDP3ursAA1UVvxTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplCCoroutineAccessors: P3 module read_requirements {
// CHECK-NEXT:    method #P3.ur!read
// CHECK-SAME:        : @$s17read_requirements23ImplCCoroutineAccessorsVAA2P3A2aDP2urAA1UVvrTW
// CHECK-NEXT:    method #P3.ur!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements23ImplCCoroutineAccessorsVAA2P3A2aDP2urAA1UVvyTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplAGetSet: P1 module read_requirements {
// CHECK-NEXT:    method #P1.ubgs!read
// CHECK-SAME:        : @$s17read_requirements11ImplAGetSetVAA2P1A2aDP4ubgsAA1UVvrTW
// CHECK-NEXT:  method #P1.ubgs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements11ImplAGetSetVAA2P1A2aDP4ubgsAA1UVvyTW
// CHECK-NEXT:    method #P1.ubgs!setter
// CHECK-SAME:        : @$s17read_requirements11ImplAGetSetVAA2P1A2aDP4ubgsAA1UVvsTW
// CHECK-NEXT:    method #P1.ubgs!modify
// CHECK-SAME:        : @$s17read_requirements11ImplAGetSetVAA2P1A2aDP4ubgsAA1UVvMTW
// CHECK-NEXT:  method #P1.ubgs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements11ImplAGetSetVAA2P1A2aDP4ubgsAA1UVvxTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplBGetSet: P2 module read_requirements {
// CHECK-NEXT:    method #P2.urs!read
// CHECK-SAME:        : @$s17read_requirements11ImplBGetSetVAA2P2A2aDP3ursAA1UVvrTW
// CHECK-NEXT:  method #P2.urs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements11ImplBGetSetVAA2P2A2aDP3ursAA1UVvyTW
// CHECK-NEXT:    method #P2.urs!setter
// CHECK-SAME:        : @$s17read_requirements11ImplBGetSetVAA2P2A2aDP3ursAA1UVvsTW
// CHECK-NEXT:    method #P2.urs!modify
// CHECK-SAME:        : @$s17read_requirements11ImplBGetSetVAA2P2A2aDP3ursAA1UVvMTW
// CHECK-NEXT:  method #P2.urs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements11ImplBGetSetVAA2P2A2aDP3ursAA1UVvxTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplCGetSet: P3 module read_requirements {
// CHECK-NEXT:    method #P3.ur!read
// CHECK-SAME:        : @$s17read_requirements11ImplCGetSetVAA2P3A2aDP2urAA1UVvrTW
// CHECK-NEXT:  method #P3.ur!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements11ImplCGetSetVAA2P3A2aDP2urAA1UVvyTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplAUnsafeAddressors: P1 module read_requirements {
// CHECK-NEXT:    method #P1.ubgs!read
// CHECK-SAME:        : @$s17read_requirements21ImplAUnsafeAddressorsVAA2P1A2aDP4ubgsAA1UVvrTW
// CHECK-NEXT:    method #P1.ubgs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements21ImplAUnsafeAddressorsVAA2P1A2aDP4ubgsAA1UVvyTW
// CHECK-NEXT:    method #P1.ubgs!setter
// CHECK-SAME:        : @$s17read_requirements21ImplAUnsafeAddressorsVAA2P1A2aDP4ubgsAA1UVvsTW
// CHECK-NEXT:    method #P1.ubgs!modify
// CHECK-SAME:        : @$s17read_requirements21ImplAUnsafeAddressorsVAA2P1A2aDP4ubgsAA1UVvMTW
// CHECK-NEXT:    method #P1.ubgs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements21ImplAUnsafeAddressorsVAA2P1A2aDP4ubgsAA1UVvxTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplBUnsafeAddressors: P2 module read_requirements {
// CHECK-NEXT:    method #P2.urs!read
// CHECK-SAME:        : @$s17read_requirements21ImplBUnsafeAddressorsVAA2P2A2aDP3ursAA1UVvrTW
// CHECK-NEXT:    method #P2.urs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements21ImplBUnsafeAddressorsVAA2P2A2aDP3ursAA1UVvyTW
// CHECK-NEXT:    method #P2.urs!setter
// CHECK-SAME:        : @$s17read_requirements21ImplBUnsafeAddressorsVAA2P2A2aDP3ursAA1UVvsTW
// CHECK-NEXT:    method #P2.urs!modify
// CHECK-SAME:        : @$s17read_requirements21ImplBUnsafeAddressorsVAA2P2A2aDP3ursAA1UVvMTW
// CHECK-NEXT:    method #P2.urs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements21ImplBUnsafeAddressorsVAA2P2A2aDP3ursAA1UVvxTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table{{.*}} ImplCUnsafeAddressors: P3 module read_requirements {
// CHECK-NEXT:    method #P3.ur!read
// CHECK-SAME:        : @$s17read_requirements21ImplCUnsafeAddressorsVAA2P3A2aDP2urAA1UVvrTW
// CHECK-NEXT:    method #P3.ur!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements21ImplCUnsafeAddressorsVAA2P3A2aDP2urAA1UVvyTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_default_witness_table P1 {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #P1.ubgs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements2P1P4ubgsAA1UVvy
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #P1.ubgs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements2P1P4ubgsAA1UVvx
// CHECK-NEXT:  }

// CHECK-LABEL: sil_default_witness_table P2 {
// CHECK-NEXT:    no_default
// CHECK-NEXT:  method #P2.urs!yielding_borrow
// CHECK-SAME:      : @$s17read_requirements2P2P3ursAA1UVvy
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:  method #P2.urs!yielding_mutate
// CHECK-SAME:      : @$s17read_requirements2P2P3ursAA1UVvx
// CHECK-NEXT:  }

// CHECK-LABEL: sil_default_witness_table P3 {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #P3.ur!yielding_borrow:
// CHECK-SAME:      : @$s17read_requirements2P3P2urAA1UVvy
// CHECK-NEXT:  }
