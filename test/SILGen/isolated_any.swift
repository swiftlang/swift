// RUN: %target-swift-frontend -emit-silgen -enable-experimental-feature IsolatedAny %s -module-name test -swift-version 5 -disable-availability-checking | %FileCheck %s
// REQUIRES: concurrency

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

// TODO: the same with closures
