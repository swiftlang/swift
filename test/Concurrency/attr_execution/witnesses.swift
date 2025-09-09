// RUN: %target-swift-emit-silgen %s -swift-version 6 -target %target-swift-5.1-abi-triple | %FileCheck %s

// REQUIRES: asserts
// REQUIRES: concurrency

protocol P {
  nonisolated(nonsending) var prop: String { get async }
  nonisolated(nonsending) func fn() async
}

struct S: P {
  var prop: String {
    get async { "" }
  }

  func fn() async {
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s9witnesses21testMainActorDispatch1tyx_tYaAA1PRzlF : $@convention(thin) @async <T where T : P> (@in_guaranteed T) -> ()
// CHECK: [[MAIN_ACTOR_TYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[MAIN_ACTOR:%.*]] = apply {{.*}}([[MAIN_ACTOR_TYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[BORROWED_MAIN_ACTOR:%.*]] = begin_borrow [[MAIN_ACTOR]]
// CHECK-NEXT: hop_to_executor [[BORROWED_MAIN_ACTOR]]
// CHECK-NEXT: [[MAIN_ACTOR_COPY:%.*]] = copy_value [[BORROWED_MAIN_ACTOR]]
// CHECK-NEXT: [[ERASED_TO_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR_COPY]] : $MainActor : $MainActor, $any Actor
// CHECK-NEXT: [[CONTEXT_ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_TO_ACTOR]]
// CHECK-NEXT: [[PROP_WITNESS:%.*]] = witness_method $T, #P.prop!getter : <Self where Self : P> (Self) -> () async -> String : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> @owned String
// CHECK-NEXT: {{.*}} = apply [[PROP_WITNESS]]<T>([[CONTEXT_ISOLATION]], %0) : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> @owned String
// CHECK: hop_to_executor [[BORROWED_MAIN_ACTOR]]
// CHECK: [[MAIN_ACTOR_COPY:%.*]] = copy_value [[BORROWED_MAIN_ACTOR]]
// CHECK-NEXT: [[ERASED_TO_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR_COPY]] : $MainActor : $MainActor, $any Actor
// CHECK-NEXT: [[CONTEXT_ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_TO_ACTOR]]
// CHECK-NEXT: [[FN_WITNESS:%.*]] = witness_method $T, #P.fn : <Self where Self : P> (Self) -> () async -> () : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
// CHECK-NEXT: {{.*}} = apply [[FN_WITNESS]]<T>([[CONTEXT_ISOLATION]], %0) : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
// CHECK: } // end sil function '$s9witnesses21testMainActorDispatch1tyx_tYaAA1PRzlF'
@MainActor
func testMainActorDispatch<T: P>(t: T) async {
  _ = await t.prop
  _ = await t.fn()
}

// CHECK-LABEL: sil hidden [ossa] @$s9witnesses19testGenericExecutor1tyx_tYaAA1PRzlF : $@convention(thin) @async <T where T : P> (@in_guaranteed T) -> ()
// CHECK: [[CONTEXT_ISOLATION:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
// CHECK-NEXT: hop_to_executor [[CONTEXT_ISOLATION]]
// CHECK-NEXT: [[ISOLATION_ERASED_TO_ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-NEXT: [[PROP_WITNESS:%.*]] = witness_method $T, #P.prop!getter : <Self where Self : P> (Self) -> () async -> String : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> @owned String
// CHECK-NEXT: {{.*}} = apply [[PROP_WITNESS]]<T>([[ISOLATION_ERASED_TO_ACTOR]], %0) : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> @owned String
// CHECK-NEXT: hop_to_executor [[CONTEXT_ISOLATION]]
// CHECK: [[ISOLATION_ERASED_TO_ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-NEXT: [[FN_WITNESS:%.*]] = witness_method $T, #P.fn : <Self where Self : P> (Self) -> () async -> () : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
// CHECK-NEXT: {{.*}} = apply [[FN_WITNESS]]<T>([[ISOLATION_ERASED_TO_ACTOR]], %0) : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
// CHECK: } // end sil function '$s9witnesses19testGenericExecutor1tyx_tYaAA1PRzlF'
func testGenericExecutor<T: P>(t: T) async {
  _ = await t.prop
  _ = await t.fn()
}

actor Test {
  // CHECK-LABEL: sil hidden [ossa] @$s9witnesses4TestC4test1tyx_tYaAA1PRzlF : $@convention(method) @async <T where T : P> (@in_guaranteed T, @sil_isolated @guaranteed Test) -> ()
  // CHECK: bb0(%0 : $*T, [[ACTOR:%.*]] : @guaranteed $Test):
  // CHECK: hop_to_executor [[ACTOR]]
  // CHECK-NEXT: [[ACTOR_COPY:%.*]] = copy_value [[ACTOR]]
  // CHECK-NEXT: [[ERASED_TO_ACTOR:%.*]] = init_existential_ref [[ACTOR_COPY]] : $Test : $Test, $any Actor
  // CHECK-NEXT: [[CONTEXT_ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_TO_ACTOR]]
  // CHECK-NEXT: [[PROP_WITNESS:%.*]] = witness_method $T, #P.prop!getter : <Self where Self : P> (Self) -> () async -> String : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> @owned String
  // CHECK-NEXT: {{.*}} = apply [[PROP_WITNESS]]<T>([[CONTEXT_ISOLATION]], %0) : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> @owned String
  // CHECK: hop_to_executor [[ACTOR]]
  // CHECK: [[ACTOR_COPY:%.*]] = copy_value [[ACTOR]]
  // CHECK-NEXT: [[ERASED_TO_ACTOR:%.*]] = init_existential_ref [[ACTOR_COPY]] : $Test : $Test, $any Actor
  // CHECK-NEXT: [[CONTEXT_ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_TO_ACTOR]]
  // CHECK-NEXT: [[FN_WITNESS:%.*]] = witness_method $T, #P.fn : <Self where Self : P> (Self) -> () async -> () : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
  // CHECK-NEXT: {{.*}} = apply [[FN_WITNESS]]<T>([[CONTEXT_ISOLATION]], %0) : $@convention(witness_method: P) @async <τ_0_0 where τ_0_0 : P> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed τ_0_0) -> ()
  // CHECK: } // end sil function '$s9witnesses4TestC4test1tyx_tYaAA1PRzlF'
  func test<T: P>(t: T) async {
    _ = await t.prop
    _ = await t.fn()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s9witnesses14testDirectCall1syAA1SV_tYaF : $@convention(thin) @async (S) -> ()
// CHECK: [[MAIN_ACTOR_TYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[MAIN_ACTOR:%.*]] = apply {{.*}}([[MAIN_ACTOR_TYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[BORROWED_MAIN_ACTOR:%.*]] = begin_borrow [[MAIN_ACTOR]]
// CHECK-NEXT: hop_to_executor [[BORROWED_MAIN_ACTOR]]
// CHECK-NEXT: [[MAIN_ACTOR_COPY:%.*]] = copy_value [[BORROWED_MAIN_ACTOR]]
// CHECK-NEXT: [[ERASED_TO_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR_COPY]] : $MainActor : $MainActor, $any Actor
// CHECK-NEXT: [[CONTEXT_ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_TO_ACTOR]]
// CHECK: [[PROP_REF:%.*]] = function_ref @$s9witnesses1SV4propSSvg : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, S) -> @owned String
// CHECK-NEXT: {{.*}} = apply [[PROP_REF]]([[CONTEXT_ISOLATION]], %0) : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, S) -> @owned String
// CHECK: hop_to_executor [[BORROWED_MAIN_ACTOR]]
// CHECK: [[MAIN_ACTOR_COPY:%.*]] = copy_value [[BORROWED_MAIN_ACTOR]]
// CHECK-NEXT: [[ERASED_TO_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR_COPY]] : $MainActor : $MainActor, $any Actor
// CHECK-NEXT: [[CONTEXT_ISOLATION:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ERASED_TO_ACTOR]]
// CHECK: [[FN_REF:%.*]] = function_ref @$s9witnesses1SV2fnyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, S) -> ()
// CHECK-NEXT: {{.*}} = apply [[FN_REF]]([[CONTEXT_ISOLATION]], %0) : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, S) -> ()
// CHECK: } // end sil function '$s9witnesses14testDirectCall1syAA1SV_tYaF'
@MainActor
func testDirectCall(s: S) async {
  _ = await s.prop
  _ = await s.fn()
}
