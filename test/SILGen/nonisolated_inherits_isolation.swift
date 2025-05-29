// RUN: %target-swift-emit-silgen -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

//===----------------------------------------------------------------------===//
//                             MARK: Declarations
//===----------------------------------------------------------------------===//

class NonSendableKlass {}

func unspecifiedSyncUse(_ t: NonSendableKlass) {}
func unspecifiedAsyncUse(_ t: NonSendableKlass) async {}

//===----------------------------------------------------------------------===//
//                                MARK: Tests
//===----------------------------------------------------------------------===//

// CHECK-LABEL: // nonisolatedAsync()
// CHECK-NEXT: Isolation: caller_isolation_inheriting
// CHECK-NEXT: sil hidden [ossa] @$s30nonisolated_inherits_isolation0A5AsyncyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$s30nonisolated_inherits_isolation0A5AsyncyyYaF'
nonisolated func nonisolatedAsync() async {}

func unspecifiedAsyncCallee() async {}

// CHECK-LABEL: // unspecifiedAsync()
// CHECK-NEXT: Isolation: caller_isolation_inheriting
// CHECK-NEXT: sil hidden [ossa] @$s30nonisolated_inherits_isolation16unspecifiedAsyncyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Optional<any Actor>):
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   [[FUNC_REF:%.*]] = function_ref @$s30nonisolated_inherits_isolation22unspecifiedAsyncCalleeyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
// CHECK:   apply [[FUNC_REF]]([[ACTOR]])
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$s30nonisolated_inherits_isolation16unspecifiedAsyncyyYaF'
func unspecifiedAsync() async {
  await unspecifiedAsyncCallee()
}

struct NonisolatedStruct {
  // Do not apply it to sync initializers.
  //
  // CHECK-LABEL: // NonisolatedStruct.init(sync:)
  // CHECK-NEXT: // Isolation: unspecified
  // CHECK-NEXT: sil hidden [ossa] @$s30nonisolated_inherits_isolation17NonisolatedStructV4syncACyt_tcfC : $@convention(method) (@thin NonisolatedStruct.Type) -> NonisolatedStruct {
  // CHECK: } // end sil function '$s30nonisolated_inherits_isolation17NonisolatedStructV4syncACyt_tcfC'
  init(sync: ()) {}

  // Do apply it to sync initializers.
  //
  // CHECK-LABEL: // NonisolatedStruct.init(asynchronous:)
  // CHECK-NEXT: // Isolation: caller_isolation_inheriting
  // CHECK-NEXT: sil hidden [ossa] @$s30nonisolated_inherits_isolation17NonisolatedStructV12asynchronousACyt_tYacfC : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thin NonisolatedStruct.Type) -> NonisolatedStruct {
  // CHECK: } // end sil function '$s30nonisolated_inherits_isolation17NonisolatedStructV12asynchronousACyt_tYacfC'
  init(asynchronous: ()) async {}

  // Do not apply it to non-async methods. These do not require any changes.
  func syncMethod() {}

  // But do apply it to async methods.
  // CHECK-LABEL: // NonisolatedStruct.asyncMethod()
  // CHECK-NEXT: // Isolation: caller_isolation_inheriting
  // CHECK-NEXT: sil hidden [ossa] @$s30nonisolated_inherits_isolation17NonisolatedStructV11asyncMethodyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, NonisolatedStruct) -> () {
  // CHECK: } // end sil function '$s30nonisolated_inherits_isolation17NonisolatedStructV11asyncMethodyyYaF'
  func asyncMethod() async {}
}

// CHECK-LABEL: // useNonisolatedStruct()
// CHECK-NEXT: // Isolation: caller_isolation_inheriting
// CHECK-NEXT: sil hidden [ossa] @$s30nonisolated_inherits_isolation20useNonisolatedStructyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK: bb0([[ACTOR:%.*]] :
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   [[VALUE:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thin NonisolatedStruct.Type) -> NonisolatedStruct
// CHECK:   [[VAR_DECL:%.*]] = move_value [var_decl] [[VALUE]]
// CHECK:   [[ASYNC_CALL:%.*]] = function_ref @$s30nonisolated_inherits_isolation17NonisolatedStructV11asyncMethodyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, NonisolatedStruct) -> ()
// CHECK:   apply [[ASYNC_CALL]]([[ACTOR]], [[VAR_DECL]])
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   extend_lifetime [[VAR_DECL]]
// CHECK: } // end sil function '$s30nonisolated_inherits_isolation20useNonisolatedStructyyYaF'
func useNonisolatedStruct() async {
  // Should be disconnected.
  let x = NonisolatedStruct(sync: ())

  // When we call the async method, we should pass in the implicit parameter.
  await x.asyncMethod()
}

// CHECK-LABEL: // useNonisolatedStruct2()
// CHECK-NEXT: // Isolation: caller_isolation_inheriting
// CHECK-NEXT: sil hidden [ossa] @$s30nonisolated_inherits_isolation21useNonisolatedStruct2yyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK: bb0([[ACTOR:%.*]] :
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   [[VALUE:%.*]] = apply {{%.*}}([[ACTOR]], {{%.*}}) : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @thin NonisolatedStruct.Type) -> NonisolatedStruct
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   [[VAR_DECL:%.*]] = move_value [var_decl] [[VALUE]]
// CHECK:   apply {{%.*}}([[ACTOR]], [[VAR_DECL]]) : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, NonisolatedStruct) -> ()
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   extend_lifetime [[VAR_DECL]]
// CHECK: } // end sil function '$s30nonisolated_inherits_isolation21useNonisolatedStruct2yyYaF'
func useNonisolatedStruct2() async {
  let x = await NonisolatedStruct(asynchronous: ())

  // When we call the async method, we should pass in the implicit parameter.
  await x.asyncMethod()
}

// Make sure that we handle global actor isolated functions without adding the
// implicit parameter convention.
//
// CHECK-LABEL: // mainActorSync()
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-NEXT: sil hidden [ossa] @$s30nonisolated_inherits_isolation13mainActorSyncyyF : $@convention(thin) () -> () {
// CHECK: } // end sil function '$s30nonisolated_inherits_isolation13mainActorSyncyyF'
@MainActor func mainActorSync() {}

// CHECK-LABEL: // mainActorAsync()
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-NEXT: sil hidden [ossa] @$s30nonisolated_inherits_isolation14mainActorAsyncyyYaF : $@convention(thin) @async () -> () {
// CHECK: } // end sil function '$s30nonisolated_inherits_isolation14mainActorAsyncyyYaF'
@MainActor func mainActorAsync() async {}

@MainActor
class MainActorKlass {
  var ns = NonSendableKlass()

  // CHECK-LABEL: // MainActorKlass.callNonIsolatedWithParam()
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden [ossa] @$s30nonisolated_inherits_isolation14MainActorKlassC24callNonIsolatedWithParamyyYaF : $@convention(method) @async (@guaranteed MainActorKlass) -> () {
  // CHECK: bb0(
  // CHECK:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
  // CHECK:   [[B_ACTOR:%.*]] = begin_borrow [[ACTOR]]
  // CHECK:   hop_to_executor [[B_ACTOR]]
  // CHECK:   [[VAR_DECL:%.*]] = move_value [lexical] [var_decl]
  // CHECK:   [[B_VAR_DECL:%.*]] = begin_borrow [[VAR_DECL]]
  //   Sync call
  // CHECK:   apply {{%.*}}([[B_VAR_DECL]]) : $@convention(thin) (@guaranteed NonSendableKlass) -> ()
  // CHECK:   [[B_VAR_DECL_2:%.*]] = begin_borrow [[VAR_DECL]]
  //   Async call with marshaling to wrap the actor in Optional<any Actor>
  // CHECK:   [[B_ACTOR_COPY:%.*]] = copy_value [[B_ACTOR]]
  // CHECK:   [[B_ACTOR_COPY_EX:%.*]] = init_existential_ref [[B_ACTOR_COPY]] : $MainActor
  // CHECK:   [[B_ACTOR_COPY_EX_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[B_ACTOR_COPY_EX]]
  // CHECK:   apply {{%.*}}([[B_ACTOR_COPY_EX_OPT]], [[B_VAR_DECL_2]]) : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed NonSendableKlass) -> ()
  // CHECK:   hop_to_executor [[B_ACTOR]]
  // CHECK: } // end sil function '$s30nonisolated_inherits_isolation14MainActorKlassC24callNonIsolatedWithParamyyYaF'
  func callNonIsolatedWithParam() async {
    let n = ns
    unspecifiedSyncUse(n)
    await unspecifiedAsyncUse(n)
  }
}

struct TestVarUse {
  var test: Int {
    // CHECK-LABEL: sil hidden [ossa] @$s30nonisolated_inherits_isolation10TestVarUseV4testSivg : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, TestVarUse) -> Int
    get async {
      42
    }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s30nonisolated_inherits_isolation12testUseOfVar1tyAA04TestgE0V_tYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, TestVarUse) -> ()
// CHECK: bb0([[ISOLATION:%.*]] : @guaranteed $Optional<any Actor>, [[BASE:%.*]] : $TestVarUse)
// CHECK:   [[GETTER:%.*]] = function_ref @$s30nonisolated_inherits_isolation10TestVarUseV4testSivg : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, TestVarUse) -> Int
// CHECK:   {{.*}} = apply [[GETTER]]([[ISOLATION]], [[BASE]])
// CHECK: } // end sil function '$s30nonisolated_inherits_isolation12testUseOfVar1tyAA04TestgE0V_tYaF'
func testUseOfVar(t: TestVarUse) async {
  _ = await t.test
}
