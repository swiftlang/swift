// RUN: %target-swift-frontend -emit-silgen -enable-experimental-feature IsolatedAny %s -module-name test -swift-version 5 -disable-availability-checking | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: asserts

// CHECK-LABEL: sil hidden [ossa] @$s4test8callSync2fnyyyYAXE_tYaF
// CHECK:         [[NIL_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// CHECK-NEXT:    hop_to_executor [[NIL_EXECUTOR]]
// CHECK-NEXT:    [[FN_COPY:%.*]] = copy_value %0 : $@isolated(any) @noescape @callee_guaranteed () -> ()
// CHECK-NEXT:    [[FN_BORROW1:%.*]] = begin_borrow [[FN_COPY]] :
// CHECK-NEXT:    [[ISOLATION:%.*]] = function_extract_isolation [[FN_BORROW1]] :
// CHECK-NEXT:    hop_to_executor [[ISOLATION]] : $Optional<any Actor>
// CHECK-NEXT:    [[FN_BORROW2:%.*]] = begin_borrow [[FN_COPY]] :
// CHECK-NEXT:    apply [[FN_BORROW2]]()
// CHECK-NEXT:    end_borrow [[FN_BORROW2]]
// CHECK-NEXT:    end_borrow [[FN_BORROW1]]
// CHECK-NEXT:    hop_to_executor [[NIL_EXECUTOR]]
func callSync(fn: @isolated(any) () -> ()) async {
  await fn()
}

// CHECK-LABEL: sil hidden [ossa] @$s4test9callAsync2fnyyyYaYAXE_tYaF
// CHECK:         [[NIL_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
// CHECK-NEXT:    hop_to_executor [[NIL_EXECUTOR]]
// CHECK-NEXT:    [[FN_COPY:%.*]] = copy_value %0 : $@isolated(any) @noescape @async @callee_guaranteed () -> ()
// CHECK-NEXT:    [[FN_BORROW2:%.*]] = begin_borrow [[FN_COPY]] :
// CHECK-NEXT:    apply [[FN_BORROW2]]()
// CHECK-NEXT:    end_borrow [[FN_BORROW2]]
// CHECK-NEXT:    hop_to_executor [[NIL_EXECUTOR]]
func callAsync(fn: @isolated(any) () async -> ()) async {
  await fn()
}

// CHECK-LABEL: sil hidden [ossa] @$s4test22convertFromNonIsolated2fnyyYaYAcyyYac_tF
// CHECK:       bb0(%0 : @guaranteed $@async @callee_guaranteed () -> ()):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[FN_COPY:%.*]] = copy_value %0 :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.none
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[THUNK:%.*]] = function_ref @$sIegH_IeAgH_TR : $@convention(thin) @async (@guaranteed Optional<any Actor>, @guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK-NEXT:    [[THUNKED_FN:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[THUNK]]([[ISOLATION]], [[FN_COPY]])
// CHECK-NEXT:    return [[THUNKED_FN]] : $@isolated(any) @async @callee_guaranteed () -> ()
func convertFromNonIsolated(fn: @escaping () async -> ())
    -> @isolated(any) () async -> () {
  return fn
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIegH_IeAgH_TR :
// CHECK:       bb0(%0 : @guaranteed $Optional<any Actor>, %1 : @guaranteed $@async @callee_guaranteed () -> ()):
// CHECK-NEXT:    apply %1()
// CHECK-NEXT:    tuple
// CHECK-NEXT:    return

// CHECK-LABEL: sil hidden [ossa] @$s4test20convertFromMainActor2fnyyYaYAcyyYaScMYcc_tF
// CHECK:       bb0(%0 : @guaranteed $@async @callee_guaranteed () -> ()):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[FN_COPY:%.*]] = copy_value %0 :
// CHECK-NEXT:    [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[MAIN_ACTOR_SHARED_FN:%.]] = function_ref @$sScM6sharedScMvgZ :
// CHECK-NEXT:    [[MAIN_ACTOR:%.*]] = apply [[MAIN_ACTOR_SHARED_FN]]([[MAIN_ACTOR_METATYPE]])
// CHECK-NEXT:    [[ERASED_MAIN_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR]] :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_MAIN_ACTOR]] : $any Actor
//   FIXME: the fact that this uses the same reabstraction thunk as above is okay
//   in this case but extremely wrong in general.  For example, if we were doing
//   this conversion in a nested position, we'd end up using the wrong conversion.
//   We need to be mangling the other kinds of isolation into impl-function-type,
//   which also means we need some sort of solution for the inexpressible-in-types
//   cases like actor-instance isolation.
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[THUNK:%.*]] = function_ref @$sIegH_IeAgH_TR : $@convention(thin) @async (@guaranteed Optional<any Actor>, @guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK-NEXT:    [[THUNKED_FN:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[THUNK]]([[ISOLATION]], [[FN_COPY]])
// CHECK-NEXT:    return [[THUNKED_FN]] : $@isolated(any) @async @callee_guaranteed () -> ()
func convertFromMainActor(fn: @escaping @MainActor () async -> ())
    -> @isolated(any) () async -> () {
  return fn
}

// CHECK-LABEL: sil hidden [ossa] @$s4test36convertFromMainActorWithOtherChanges2fnSiSgyYaYAcSiyYaScMYcc_tF
// CHECK:       bb0(%0 : @guaranteed $@async @callee_guaranteed () -> Int):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[FN_COPY:%.*]] = copy_value %0 :
// CHECK-NEXT:    [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[MAIN_ACTOR_SHARED_FN:%.]] = function_ref @$sScM6sharedScMvgZ :
// CHECK-NEXT:    [[MAIN_ACTOR:%.*]] = apply [[MAIN_ACTOR_SHARED_FN]]([[MAIN_ACTOR_METATYPE]])
// CHECK-NEXT:    [[ERASED_MAIN_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR]] :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_MAIN_ACTOR]] : $any Actor
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[THUNK:%.*]] = function_ref @$sSiIegHd_SiSgIeAgHd_TR : 
// CHECK-NEXT:    [[THUNKED_FN:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[THUNK]]([[ISOLATION]], [[FN_COPY]])
// CHECK-NEXT:    return [[THUNKED_FN]] : $@isolated(any) @async @callee_guaranteed () -> Optional<Int>
func convertFromMainActorWithOtherChanges(fn: @escaping @MainActor () async -> Int)
    -> @isolated(any) () async -> Int? {
  return fn
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSiIegHd_SiSgIeAgHd_TR :
// CHECK:       bb0(%0 : @guaranteed $Optional<any Actor>, %1 : @guaranteed $@async @callee_guaranteed () -> Int):
// CHECK-NEXT:    [[INT:%.*]] = apply %1()
// CHECK-NEXT:    [[SOME_INT:%.*]] = enum $Optional<Int>, #Optional.some!enumelt, [[INT]] : $Int
// CHECK-NEXT:    return [[SOME_INT]] : $Optional<Int>

//   We can eliminate @isolated(any) without a thunk.
// CHECK-LABEL: sil hidden [ossa] @$s4test20convertToNonIsolated2fnyyYacyyYaYAc_tF
// CHECK:       bb0(%0 : @guaranteed $@isolated(any) @async @callee_guaranteed () -> ()):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[FN_COPY:%.*]] = copy_value %0 :
// CHECK-NEXT:    [[FN_CONVERTED:%.*]] = convert_function [[FN_COPY]] : $@isolated(any) @async @callee_guaranteed () -> () to $@async @callee_guaranteed () -> ()
// CHECK-NEXT:    return [[FN_CONVERTED]] :
func convertToNonIsolated(fn: @escaping @isolated(any) () async -> ())
    -> () async -> () {
  return fn
}

//   If we do require a thunk, make sure we do the right thing when erasing
//   @isolated(any).
// CHECK-LABEL: sil hidden [ossa] @$s4test36convertToNonIsolatedWithOtherChanges2fnSiSgyYacSiyYaYAc_tF
// CHECK:       bb0(%0 : @guaranteed $@isolated(any) @async @callee_guaranteed () -> Int):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[FN_COPY:%.*]] = copy_value %0 :
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[THUNK:%.*]] = function_ref @$sSiIeAgHd_SiSgIegHd_TR : 
// CHECK-NEXT:    [[THUNKED_FN:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[FN_COPY]])
// CHECK-NEXT:    return [[THUNKED_FN]] : $@async @callee_guaranteed () -> Optional<Int>

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSiIeAgHd_SiSgIegHd_TR
// CHECK:       bb0(%0 : @guaranteed $@isolated(any) @async @callee_guaranteed () -> Int):
// CHECK-NEXT:    [[INT:%.*]] = apply %0()
// CHECK-NEXT:    [[SOME_INT:%.*]] = enum $Optional<Int>, #Optional.some!enumelt, [[INT]] : $Int
// CHECK-NEXT:    return [[SOME_INT]] : $Optional<Int>

func convertToNonIsolatedWithOtherChanges(fn: @escaping @isolated(any) () async -> Int) -> () async -> Int? {
  return fn
}

/*-- Sync closures --*/

func syncAction() {}

func takeSyncIsolatedAny(fn: @escaping @isolated(any) () -> ()) {}
func takeInheritingSyncIsolatedAny(@_inheritActorContext fn: @escaping @isolated(any) () -> ()) {}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A27EraseSyncNonIsolatedClosureyyF
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test0A27EraseSyncNonIsolatedClosureyyFyycfU_ :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test19takeSyncIsolatedAny2fnyyyYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return
func testEraseSyncNonIsolatedClosure() {
  takeSyncIsolatedAny {
    syncAction()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A25EraseSyncMainActorClosureyyF
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test0A25EraseSyncMainActorClosureyyFyyScMYccfU_ :
// CHECK-NEXT:    [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[MAIN_ACTOR_SHARED_FN:%.]] = function_ref @$sScM6sharedScMvgZ :
// CHECK-NEXT:    [[MAIN_ACTOR:%.*]] = apply [[MAIN_ACTOR_SHARED_FN]]([[MAIN_ACTOR_METATYPE]])
// CHECK-NEXT:    [[ERASED_MAIN_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR]] :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_MAIN_ACTOR]] : $any Actor
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test19takeSyncIsolatedAny2fnyyyYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return
func testEraseSyncMainActorClosure() {
  takeSyncIsolatedAny { @MainActor in
    syncAction()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A37EraseInheritingSyncNonIsolatedClosureyyF
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test0A37EraseInheritingSyncNonIsolatedClosureyyFyycfU_ :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test29takeInheritingSyncIsolatedAny2fnyyyYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return
func testEraseInheritingSyncNonIsolatedClosure() {
  takeInheritingSyncIsolatedAny {
    syncAction()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A35EraseInheritingSyncMainActorClosureyyF
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test0A35EraseInheritingSyncMainActorClosureyyFyyScMYccfU_ :
// CHECK-NEXT:    [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[MAIN_ACTOR_SHARED_FN:%.]] = function_ref @$sScM6sharedScMvgZ :
// CHECK-NEXT:    [[MAIN_ACTOR:%.*]] = apply [[MAIN_ACTOR_SHARED_FN]]([[MAIN_ACTOR_METATYPE]])
// CHECK-NEXT:    [[ERASED_MAIN_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR]] :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_MAIN_ACTOR]] : $any Actor
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test29takeInheritingSyncIsolatedAny2fnyyyYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return
@MainActor
func testEraseInheritingSyncMainActorClosure() {
  takeInheritingSyncIsolatedAny { @MainActor in
    syncAction()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC0a19EraseInheritingSyncC7ClosureyyF
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test7MyActorC0a19EraseInheritingSyncC7ClosureyyFyycfU_ : $@convention(thin) (@guaranteed Optional<any Actor>, @guaranteed MyActor) -> ()
// CHECK-NEXT:    [[CAPTURE:%.*]] = copy_value %0 : $MyActor
// CHECK-NEXT:    [[CAPTURE_FOR_ISOLATION:%.*]] = copy_value [[CAPTURE]] : $MyActor
// CHECK-NEXT:    [[ISOLATION_OBJECT:%.*]] = init_existential_ref [[CAPTURE_FOR_ISOLATION]] : $MyActor : $MyActor, $any Actor
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ISOLATION_OBJECT]] : $any Actor
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]], [[CAPTURE]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test29takeInheritingSyncIsolatedAny2fnyyyYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
actor MyActor {
  func testEraseInheritingSyncActorClosure() {
    takeInheritingSyncIsolatedAny {
      // Note that the closure has to actually *reference* `self` to be
      // formally isolated to it; it can't just include it in the captures
      // list.  That's the rule, for better or worse.
      self.syncAction()
    }
  }

  func syncAction() {}
}

/*-- Async closures --*/

func asyncAction() async {}

func takeAsyncIsolatedAny(fn: @escaping @isolated(any) () async -> ()) {}
func takeInheritingAsyncIsolatedAny(@_inheritActorContext fn: @escaping @isolated(any) () async -> ()) {}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A28EraseAsyncNonIsolatedClosureyyF
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test0A28EraseAsyncNonIsolatedClosureyyFyyYacfU_ :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test20takeAsyncIsolatedAny2fnyyyYaYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return
func testEraseAsyncNonIsolatedClosure() {
  takeAsyncIsolatedAny {
    await asyncAction()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A26EraseAsyncMainActorClosureyyF
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test0A26EraseAsyncMainActorClosureyyFyyYacfU_ :
// CHECK-NEXT:    [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[MAIN_ACTOR_SHARED_FN:%.]] = function_ref @$sScM6sharedScMvgZ :
// CHECK-NEXT:    [[MAIN_ACTOR:%.*]] = apply [[MAIN_ACTOR_SHARED_FN]]([[MAIN_ACTOR_METATYPE]])
// CHECK-NEXT:    [[ERASED_MAIN_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR]] :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_MAIN_ACTOR]] : $any Actor
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test20takeAsyncIsolatedAny2fnyyyYaYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return
func testEraseAsyncMainActorClosure() {
  takeAsyncIsolatedAny { @MainActor in
    await asyncAction()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A38EraseInheritingAsyncNonIsolatedClosureyyF
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test0A38EraseInheritingAsyncNonIsolatedClosureyyFyyYacfU_ :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test30takeInheritingAsyncIsolatedAny2fnyyyYaYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return
func testEraseInheritingAsyncNonIsolatedClosure() {
  takeInheritingAsyncIsolatedAny {
    await asyncAction()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A36EraseInheritingAsyncMainActorClosureyyF
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test0A36EraseInheritingAsyncMainActorClosureyyFyyYacfU_ :
// CHECK-NEXT:    [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[MAIN_ACTOR_SHARED_FN:%.]] = function_ref @$sScM6sharedScMvgZ :
// CHECK-NEXT:    [[MAIN_ACTOR:%.*]] = apply [[MAIN_ACTOR_SHARED_FN]]([[MAIN_ACTOR_METATYPE]])
// CHECK-NEXT:    [[ERASED_MAIN_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR]] :
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_MAIN_ACTOR]] : $any Actor
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test30takeInheritingAsyncIsolatedAny2fnyyyYaYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    return
@MainActor
func testEraseInheritingAsyncMainActorClosure() {
  takeInheritingAsyncIsolatedAny { @MainActor in
    await asyncAction()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC0a20EraseInheritingAsyncC7ClosureyyF
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test7MyActorC0a20EraseInheritingAsyncC7ClosureyyFyyYacfU_ : $@convention(thin) @async (@guaranteed Optional<any Actor>, @guaranteed MyActor) -> ()
// CHECK-NEXT:    [[CAPTURE:%.*]] = copy_value %0 : $MyActor
// CHECK-NEXT:    [[CAPTURE_FOR_ISOLATION:%.*]] = copy_value [[CAPTURE]] : $MyActor
// CHECK-NEXT:    [[ISOLATION_OBJECT:%.*]] = init_existential_ref [[CAPTURE_FOR_ISOLATION]] : $MyActor : $MyActor, $any Actor
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ISOLATION_OBJECT]] : $any Actor
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]], [[CAPTURE]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test30takeInheritingAsyncIsolatedAny2fnyyyYaYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
extension MyActor {
  func testEraseInheritingAsyncActorClosure() {
    takeInheritingAsyncIsolatedAny {
      // Note that the closure has to actually *reference* `self` to be
      // formally isolated to it; it can't just include it in the captures
      // list.  That's the rule, for better or worse.
      await self.asyncAction()
    }
  }

  func asyncAction() async {}
}

func takeInheritingAsyncIsolatedAny_optionalResult(@_inheritActorContext fn: @escaping @isolated(any) () async -> Int?) {}

// Test that we correctly handle isolation erasure from closures even when
// we can't completely apply the conversion as a peephole.
// CHECK-LABEL: sil hidden [ossa] @$s4test7MyActorC0a20EraseInheritingAsyncC18Closure_noPeepholeyyF
//   We emit the closure itself with just the erasure conversion.
// CHECK:         // function_ref closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test7MyActorC0a20EraseInheritingAsyncC18Closure_noPeepholeyyFSiyYacfU_ : $@convention(thin) @async (@guaranteed Optional<any Actor>, @guaranteed MyActor) -> Int
// CHECK-NEXT:    [[CAPTURE:%.*]] = copy_value %0 : $MyActor
// CHECK-NEXT:    [[CAPTURE_FOR_ISOLATION:%.*]] = copy_value [[CAPTURE]] : $MyActor
// CHECK-NEXT:    [[ISOLATION_OBJECT:%.*]] = init_existential_ref [[CAPTURE_FOR_ISOLATION]] : $MyActor : $MyActor, $any Actor
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ISOLATION_OBJECT]] : $any Actor
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[CLOSURE_FN]]([[ISOLATION]], [[CAPTURE]])
//   This is the general function-conversion path, so it doesn't realize
//   we already know the isolation locally.  This is not worth peepholing in
//   SILGen, but it should be optimizable in the SIL pipeline.
// CHECK-NEXT:    [[CLOSURE_BORROW:%.*]] = begin_borrow [[CLOSURE]]
// CHECK-NEXT:    [[CLOSURE_ISOLATION_BORROW:%.*]] = function_extract_isolation [[CLOSURE_BORROW]]
// CHECK-NEXT:    [[CLOSURE_ISOLATION:%.*]] = copy_value [[CLOSURE_ISOLATION_BORROW]]
// CHECK-NEXT:    end_borrow [[CLOSURE_BORROW]]
//   Apply the converison thunk.
// CHECK-NEXT:    // function_ref thunk
// CHECK-NEXT:    [[THUNK_FN:%.*]] = function_ref
// CHECK-NEXT:    [[CONVERTED_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[THUNK_FN]]([[CLOSURE_ISOLATION]], [[CLOSURE]])
//   Call the final function.
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test45takeInheritingAsyncIsolatedAny_optionalResult2fnySiSgyYaYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CONVERTED_CLOSURE]])
// CHECK-NEXT:    destroy_value [[CONVERTED_CLOSURE]]
extension MyActor {
  func testEraseInheritingAsyncActorClosure_noPeephole() {
    // The conversion from an Int-returning closure to an Int?-returning
    // closure currently can't be peepholed.  If you change that, please
    // make this test do some other conversion that can't be peepholed,
    // unless you literally make everything peepholable.
    takeInheritingAsyncIsolatedAny_optionalResult {
      () -> Int in

      await self.asyncAction()

      return 0
    }
  }
}

/*-- Partial applications --*/

//   FIXME: this is wrong; we need to capture the actor value
// CHECK-LABEL: sil hidden [ossa] @$s4test0A41EraseAsyncActorIsolatedPartialApplication1ayAA02MyD0C_tF
// CHECK:         // function_ref implicit closure #1
// CHECK-NEXT:    [[CLOSURE_FN:%.*]] = function_ref @$s4test0A41EraseAsyncActorIsolatedPartialApplication1ayAA02MyD0C_tFyyYacAEYicfu_ : $@convention(thin) (@sil_isolated @guaranteed MyActor) -> @owned @async @callee_guaranteed () -> ()
// CHECK-NEXT:    [[FN:%.*]] = apply [[CLOSURE_FN]](%0)
// CHECK-NEXT:    [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-NEXT:    // function_ref thunk
// CHECK-NEXT:    [[THUNK_FN:%.*]] = function_ref @$sIegH_IeAgH_TR :
// CHECK-NEXT:    [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [isolated_any] [[THUNK_FN]]([[ISOLATION]], [[FN]])
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[TAKE_FN:%.*]] = function_ref @$s4test20takeAsyncIsolatedAny2fnyyyYaYAc_tF
// CHECK-NEXT:    apply [[TAKE_FN]]([[CLOSURE]])
// CHECK-NEXT:    destroy_value [[CLOSURE]]
func testEraseAsyncActorIsolatedPartialApplication(a: MyActor) {
  takeAsyncIsolatedAny(fn: a.asyncAction)
}
