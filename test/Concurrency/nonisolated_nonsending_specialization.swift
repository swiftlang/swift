// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify %s | %FileCheck %s

nonisolated(nonsending) func withValue(_ fn: nonisolated(nonsending) () async -> Void) async {
}

nonisolated(nonsending) func withValueWithArgument<T>(_ fn: nonisolated(nonsending) (T) async throws -> Void) async {
}

nonisolated(nonsending) func withValueGeneric<R>(_ fn: nonisolated(nonsending) () async throws -> R?) async throws {
}

func asyncTest(_ body: () async -> Void) async {
}

func throwingTest(_ body: () async -> Void) async throws {
}

func syncTest(_ body: () async -> Void) {
}

@MainActor func isolatedTest() {
}

@MainActor func isolatedTest(_ body: () async -> Void) async {
}

func dynamicIsolation(isolation: isolated (any Actor)? = nil, _ body: () async throws -> Void) async throws {
}

// CHECK: // testConcurrentIsolation<A>(_:body:)
// CHECK: // Isolation: unspecified
// CHECK-LABEL: sil hidden @$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalF : $@convention(thin) @async <U> (@in_guaranteed U, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
// ---- closure #1-2
// CHECK:  [[CLOSURE_1:%.*]] = function_ref @$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyYaXEfU_ : $@convention(thin) @async (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
// CHECK:  [[CLOSURE_1_WITH_BODY:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[CLOSURE_1]](%1) : $@convention(thin) @async (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
// CHECK:  [[CLOSURE_1:%.*]] = mark_dependence [[CLOSURE_1_WITH_BODY]] on %1
// CHECK:  [[CONVERTED_CLOSURE_1:%.*]] = convert_function [[CLOSURE_1]] to $@caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// CHECK:  [[WITH_VALUE:%.*]] = function_ref @$s37nonisolated_nonsending_specialization9withValueyyyyYaYCXEYaF : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
// CHECK:  apply [[WITH_VALUE]]({{.*}}, [[CONVERTED_CLOSURE_1]])
// ---- closure #3-5
// CHECK:  [[CLOSURE_3:%.*]] = function_ref @$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFySiYaXEfU1_ : $@convention(thin) @async @substituted <τ_0_0> (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error for <Int>
// CHECK:  [[CLOSURE_3_WITH_BODY:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[CLOSURE_3]](%1) : $@convention(thin) @async @substituted <τ_0_0> (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error for <Int>
// CHECK:  [[CLOSURE_3:%.*]] = mark_dependence [[CLOSURE_3_WITH_BODY]] on %1
// CHECK:  [[CONVERTED_CLOSURE_3:%.*]] = convert_function [[CLOSURE_3]] to $@caller_isolated @noescape @async @callee_guaranteed @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0) -> @error any Error for <Int>
// CHECK:  [[WITH_VALUE_WITH_ARGUMENT:%.*]] = function_ref @$s37nonisolated_nonsending_specialization21withValueWithArgumentyyyxYaKYCXEYalF : $@convention(thin) @caller_isolated @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @caller_isolated @noescape @async @callee_guaranteed @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0) -> @error any Error for <τ_0_0>) -> ()
// CHECK:  apply [[WITH_VALUE_WITH_ARGUMENT]]<Int>({{.*}}, [[CONVERTED_CLOSURE_3]])
// CHECK: } // end sil function '$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalF'                                                                                                                                        
func testConcurrentIsolation<U>(_: U, body: () async -> Void) async {
  // CHECK: // closure #1 in testConcurrentIsolation<A>(_:body:)
  // CHECK: // Isolation: @concurrent
  // CHECK-LABEL: sil private @$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyYaXEfU_ : $@convention(thin) @async (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
  // CHECK: bb0([[IGNORED_ISOLATION:%.*]] : $Builtin.ImplicitActor, [[BODY:%.*]] : @closureCapture $@noescape @async @callee_guaranteed () -> ()):
  // CHECK:   [[ASYNC_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization9asyncTestyyyyYaXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[ASYNC_TEST]]([[BODY]]) : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyYaXEfU_'
  await withValue {
    await asyncTest(body) // Ok
  }

  // CHECK: // closure #2 in testConcurrentIsolation<A>(_:body:)
  // CHECK: // Isolation: @concurrent
  // CHECK-LABEL: sil private @$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyYaXEfU0_ : $@convention(thin) @async (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
  // CHECK: bb0([[IGNORED_ISOLATION:%.*]] : $Builtin.ImplicitActor, [[BODY:%.*]] : @closureCapture $@noescape @async @callee_guaranteed () -> ()):
  // CHECK-NOT: hop_to_executor {{.*}}
  // CHECK:   [[SYNC_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization8syncTestyyyyYaXEF : $@convention(thin) (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[SYNC_TEST]]([[BODY]]) : $@convention(thin) (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyYaXEfU0_'
  await withValue {
    syncTest(body) // Ok
  }

  // CHECK: // closure #3 in testConcurrentIsolation<A>(_:body:)
  // CHECK: // Isolation: @concurrent
  // CHECK-LABEL: sil private @$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFySiYaXEfU1_ : $@convention(thin) @async @substituted <τ_0_0> (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error for <Int> {
  // CHECK: bb0([[IGNORED_ISOLATION:%.*]] : $Builtin.ImplicitActor, [[V:%.*]] : $*Int, [[BODY:%.*]] : @closureCapture $@noescape @async @callee_guaranteed () -> ()):
  // CHECK-NOT: hop_to_executor {{.*}}
  // CHECK:   [[ASYNC_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization9asyncTestyyyyYaXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[ASYNC_TEST]]([[BODY]]) : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFySiYaXEfU1_'
  await withValueWithArgument { (v: Int) in
    await asyncTest(body) // Ok
  }

  // CHECK: // closure #4 in testConcurrentIsolation<A>(_:body:)
  // CHECK: // Isolation: @concurrent
  // CHECK-LABEL: sil private @$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyt_tYaKXEfU2_ : $@convention(thin) @async @substituted <τ_0_0> (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error for <()> {
  // CHECK: bb0([[IGNORED_ISOLATION:%.*]] : $Builtin.ImplicitActor, [[V:%.*]] : $*(), [[BODY:%.*]] : @closureCapture $@noescape @async @callee_guaranteed () -> ()):
  // CHECK-NOT: hop_to_executor {{.*}}
  // CHECK:   [[THROWING_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization12throwingTestyyyyYaXEYaKF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error
  // CHECK:   try_apply [[THROWING_TEST]]([[BODY]]) : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error, normal bb1, error bb2
  // CHECK: } // end sil function '$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyt_tYaKXEfU2_'
  await withValueWithArgument { (v: Void) in
    try await throwingTest(body) // Ok
  }

  // CHECK: // closure #5 in testConcurrentIsolation<A>(_:body:)
  // CHECK: // Isolation: @concurrent
  // CHECK-LABEL: sil private @$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFySi_SSxt_tYaXEfU3_ : $@convention(thin) @async <U> (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed (Int, String, U), @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error {
  // CHECK: bb0([[IGNORED_ISOLATION:%.*]] : $Builtin.ImplicitActor, [[V:%.*]] : $*(Int, String, U), [[BODY:%.*]] : @closureCapture $@noescape @async @callee_guaranteed () -> ()):
  // CHECK-NOT: hop_to_executor {{.*}}
  // CHECK:   [[SYNC_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization8syncTestyyyyYaXEF : $@convention(thin) (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[SYNC_TEST]]([[BODY]]) : $@convention(thin) (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFySi_SSxt_tYaXEfU3_'
  await withValueWithArgument { (v: (Int, String, U)) in
    syncTest(body) // Ok
  }

  // CHECK: // closure #6 in testConcurrentIsolation<A>(_:body:)
  // CHECK: // Isolation: @concurrent
  // CHECK-LABEL: sil private @$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyYaXEfU4_ : $@convention(thin) @async (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
  // CHECK: bb0([[IGNORED_ISOLATION:%.*]] : $Builtin.ImplicitActor, [[BODY:%.*]] : @closureCapture $@noescape @async @callee_guaranteed () -> ()):
  // CHECK:   [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
  // CHECK-NOT: hop_to_executor {{.*}}
  // CHECK:   [[ASYNC_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization9asyncTestyyyyYaXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[ASYNC_TEST]]([[BODY]]) : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   hop_to_executor [[ISOLATION]]
  // CHECK:   [[ISOLATED_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization12isolatedTestyyF : $@convention(thin) () -> ()
  // CHECK:   [[MAIN_ACTOR_TYPE:%.*]] = metatype $@thick MainActor.Type
  // CHECK:   [[MAIN_ACTOR_EXEC_REF:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
  // CHECK:   [[MAIN_ACTOR:%.*]] = apply [[MAIN_ACTOR_EXEC_REF]]([[MAIN_ACTOR_TYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
  // CHECK:   hop_to_executor [[MAIN_ACTOR]]
  // CHECK:   apply [[ISOLATED_TEST]]() : $@convention(thin) () -> ()
  // CHECK: } // end sil function '$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyYaXEfU4_'
  await withValue {
    await asyncTest(body) // Ok
    await isolatedTest()  // Ok
  }

  // CHECK: // closure #7 in testConcurrentIsolation<A>(_:body:)
  // CHECK: // Isolation: @concurrent
  // CHECK: sil private @$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyYaXEfU5_ : $@convention(thin) @async (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
  // CHECK: bb0([[IGNORED_ISOLATION:%.*]] : $Builtin.ImplicitActor, [[BODY:%.*]] : @closureCapture $@noescape @async @callee_guaranteed () -> ()):
  // CHECK:   [[ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
  // CHECK-NOT: hop_to_executor {{.*}}
  // CHECK:   [[ASYNC_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization9asyncTestyyyyYaXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[ASYNC_TEST]]([[BODY]]) : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   hop_to_executor [[ISOLATION]]
  // CHECK:   [[SYNC_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization8syncTestyyyyYaXEF : $@convention(thin) (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[SYNC_TEST]]([[BODY]]) : $@convention(thin) (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$s37nonisolated_nonsending_specialization23testConcurrentIsolation_4bodyyx_yyYaXEtYalFyyYaXEfU5_'
  await withValue {
    await asyncTest(body) // Ok
    syncTest(body) // Ok
  }
}

// CHECK: // testMainActorIsolation(body:)
// CHECK: // Isolation: global_actor. type: MainActor
// CHECK-LABEL: sil hidden @$s37nonisolated_nonsending_specialization22testMainActorIsolation4bodyyyyYaXE_tYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
// CHECK:  [[CLOSURE:%.*]] = function_ref @$s37nonisolated_nonsending_specialization22testMainActorIsolation4bodyyyyYaXE_tYaFyyYaXEfU_ : $@convention(thin) @async (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
// CHECK:  [[CLOSURE_WITH_BODY:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[CLOSURE]](%0) : $@convention(thin) @async (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
// CHECK:  [[CLOSURE:%.*]] = mark_dependence [[CLOSURE_WITH_BODY]] on %0
// CHECK:  convert_function [[CLOSURE]] to $@caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// CHECK: } // end sil function '$s37nonisolated_nonsending_specialization22testMainActorIsolation4bodyyyyYaXE_tYaF'
@MainActor func testMainActorIsolation(body: () async -> Void) async {
  // CHECK: // closure #1 in testMainActorIsolation(body:)
  // CHECK: // Isolation: global_actor. type: MainActor
  // CHECK: sil private @$s37nonisolated_nonsending_specialization22testMainActorIsolation4bodyyyyYaXE_tYaFyyYaXEfU_ : $@convention(thin) @async (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
  // CHECK: bb0([[IGNORED_ISOLATION:%.*]] : $Builtin.ImplicitActor, [[BODY:%.*]] : @closureCapture $@noescape @async @callee_guaranteed () -> ()):
  // CHECK-NOT: hop_to_executor {{.*}}
  // CHECK:   [[MAIN_ACTOR:%.*]] = apply [[MAIN_ACTOR_REF:%.*]]({{.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
  // CHECK:   [[ISOLATED_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization12isolatedTestyyyyYaXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[ISOLATED_TEST]]([[BODY]]) : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   hop_to_executor [[MAIN_ACTOR]]
  // CHECK:   [[SYNC_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization8syncTestyyyyYaXEF : $@convention(thin) (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   %11 = apply [[SYNC_TEST]]([[BODY]]) : $@convention(thin) (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$s37nonisolated_nonsending_specialization22testMainActorIsolation4bodyyyyYaXE_tYaFyyYaXEfU_'
  await withValue {
    await isolatedTest(body) // Ok
    syncTest(body) // Ok
  }
}

// CHECK: // testDynamicIsolation(isolation:body:)
// CHECK: // Isolation: actor_instance. name: 'isolation'
// CHECK-LABEL: sil hidden @$s37nonisolated_nonsending_specialization20testDynamicIsolation9isolation4bodyyScA_pSgYi_yyYaXEtYaF : $@convention(thin) @async (@sil_isolated @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[ISOLATION:%.*]] : $Optional<any Actor>, [[BODY:%.*]] : $@noescape @async @callee_guaranteed () -> ()):
// CHECK:  [[CLOSURE:%.*]] = function_ref @$s37nonisolated_nonsending_specialization20testDynamicIsolation9isolation4bodyyScA_pSgYi_yyYaXEtYaFyyt_tYaKXEfU_ : $@convention(thin) @async @substituted <τ_0_0> (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0, @sil_isolated @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error for <()>
// CHECK:  [[PARTIALLY_APPLIED:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[CLOSURE]]([[ISOLATION]], [[BODY]]) : $@convention(thin) @async @substituted <τ_0_0> (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0, @sil_isolated @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error for <()>
// CHECK:  [[CLOSURE_REF:%.*]] = mark_dependence [[PARTIALLY_APPLIED]] on [[ISOLATION]]
// CHECK:  [[COMPLETE_CLOSURE:%.*]] = mark_dependence [[CLOSURE_REF]] on [[BODY]]
// CHECK:  [[CONVERTED_CLOSURE:%.*]] = convert_function [[COMPLETE_CLOSURE]] to $@caller_isolated @noescape @async @callee_guaranteed @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0) -> @error any Error for <()>
// CHECK:  apply {{.*}}<()>({{.*}}, [[CONVERTED_CLOSURE:%.*]]) : $@convention(thin) @caller_isolated @async <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @caller_isolated @noescape @async @callee_guaranteed @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0) -> @error any Error for <τ_0_0>) -> ()
// CHECK: } // end sil function '$s37nonisolated_nonsending_specialization20testDynamicIsolation9isolation4bodyyScA_pSgYi_yyYaXEtYaF'
func testDynamicIsolation(isolation: isolated (any Actor)?, body: () async -> Void) async {
  // CHECK: // closure #1 in testDynamicIsolation(isolation:body:)
  // CHECK: // Isolation: actor_instance. name: 'isolation'
  // CHECK: sil private @$s37nonisolated_nonsending_specialization20testDynamicIsolation9isolation4bodyyScA_pSgYi_yyYaXEtYaFyyt_tYaKXEfU_ : $@convention(thin) @async @substituted <τ_0_0> (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed τ_0_0, @sil_isolated @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error for <()> {
  // CHECK: bb0([[IGNORED_ISOLATION:%.*]] : $Builtin.ImplicitActor, [[V:%.*]] : $*(), [[DYNAMIC_ISOLATION:%.*]] : @closureCapture $Optional<any Actor>, [[BODY:%.*]] : @closureCapture $@noescape @async @callee_guaranteed () -> ()):
  // CHECK-NOT: hop_to_executor {{.*}}
  // CHECK:   [[THUNK:%.*]] = function_ref @$sIgH_s5Error_pIegHzo_TR : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error
  // CHECK:   [[THUNKED_BODY:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[THUNK]]([[BODY]]) : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error
  // CHECK:   [[BODY_WITH_DEPENDENCE:%.*]] = mark_dependence [[THUNKED_BODY]] on [[BODY]]
  // CHECK:   [[DYNAMIC_ISOLATION_TEST:%.*]] = function_ref @$s37nonisolated_nonsending_specialization16dynamicIsolation9isolation_yScA_pSgYi_yyYaKXEtYaKF : $@convention(thin) @async (@sil_isolated @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed () -> @error any Error) -> @error any Error
  // CHECK:   try_apply [[DYNAMIC_ISOLATION_TEST]]([[DYNAMIC_ISOLATION]], [[BODY_WITH_DEPENDENCE]]) : $@convention(thin) @async (@sil_isolated @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed () -> @error any Error) -> @error any Error, normal bb1, error bb2
  //
  // CHECK: bb1({{.*}} : $()):
  // CHECK:   hop_to_executor [[DYNAMIC_ISOLATION]]
  //
  // CHECK: bb2({{.*}} : $any Error):
  // CHECK:   hop_to_executor [[DYNAMIC_ISOLATION]]
  // CHECK: } // end sil function '$s37nonisolated_nonsending_specialization20testDynamicIsolation9isolation4bodyyScA_pSgYi_yyYaXEtYaFyyt_tYaKXEfU_'
  await withValueWithArgument { (v: Void) in
    try await dynamicIsolation(isolation: isolation, body)
  }
}

func testReabstraction(body: () async -> Void) async throws {
  // CHECK: // closure #1 in testReabstraction(body:)
  // CHECK: // Isolation: @concurrent
  // CHECK: sil private @$s37nonisolated_nonsending_specialization17testReabstraction4bodyyyyYaXE_tYaKFyyYaKXEfU_ : $@convention(thin) @async (@sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error {
  // CHECK-NOT: hop_to_executor {{.*}}
  // CHECK: } // end sil function '$s37nonisolated_nonsending_specialization17testReabstraction4bodyyyyYaXE_tYaKFyyYaKXEfU_'
  try await withValueGeneric {
    _ = 42
    try await throwingTest(body)
  }
}

// Reabstraction thunk for `testReabstraction` closure
//
// CHECK: // thunk for @caller_isolated @callee_guaranteed @async (@guaranteed Builtin.ImplicitActor) -> (@error @owned Error)
// CHECK: sil shared [transparent] [reabstraction_thunk] @$sBAs5Error_pINgHgILzo_BAytSgsAA_pIeNgHgILrzo_TR : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> @error any Error) -> (@out Optional<()>, @error any Error) {
// CHECK: bb0(%0 : $*Optional<()>, [[ISOLATION:%.*]] : $Builtin.ImplicitActor, [[CLOSURE:%.*]] : $@caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> @error any Error):
// CHECK:   hop_to_executor [[ISOLATION]]
// CHECK:   try_apply [[CLOSURE]]([[ISOLATION]]) : $@caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> @error any Error, normal bb1, error bb2
// CHECK: } // end sil function '$sBAs5Error_pINgHgILzo_BAytSgsAA_pIeNgHgILrzo_TR'
