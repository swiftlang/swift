// RUN: %target-swift-emit-silgen %s -module-name attr_execution_silgen -target %target-swift-5.1-abi-triple -DSWIFT_FIVE | %FileCheck --implicit-check-not=thunk %s
// RUN: %target-swift-emit-silgen %s -swift-version 6 -module-name attr_execution_silgen -target %target-swift-5.1-abi-triple | %FileCheck --implicit-check-not=thunk %s

// REQUIRES: asserts
// REQUIRES: concurrency

// We should only produce thunks when going to/from nonisolated(nonsending)
// since that is the only thing that makes a true ABI change since we have an
// extra parameter. For isolation purposes, we can rely on async functions
// hopping in their prolog and not need a thunk for the purposes of isolation changing.

// NOTE: We use implicit-check-not to make sure we do not create any other
// thunks beyond the ones we pattern match.

class SuperKlass {
  nonisolated(nonsending) func callerTest() async {}
  @concurrent func concurrentTest() async {}
  @MainActor func mainActorTest() async {}
}

class AllDefault : SuperKlass {
  // This doesn't need a thunk since we infer it is nonisolated(nonsending).
  override func callerTest() async {}
  override func concurrentTest() async {}
  override func mainActorTest() async {}
}

class AllConcurrent : SuperKlass {
  // CHECK-LABEL: vtable thunk for SuperKlass.callerTest() dispatching to AllConcurrent.callerTest()
  // CHECK-NEXT: sil private [thunk] [ossa] @$s21attr_execution_silgen13AllConcurrentC10callerTestyyYaFAA10SuperKlassCADyyYaFTV : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed AllConcurrent) -> () {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[PARAM:%.*]] : @guaranteed $AllConcurrent):
  // CHECK:  [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen13AllConcurrentC10callerTestyyYaF : $@convention(method) @async (@guaranteed AllConcurrent) -> ()
  // CHECK:  apply [[FUNC]]([[PARAM]])
  // CHECK: } // end sil function '$s21attr_execution_silgen13AllConcurrentC10callerTestyyYaFAA10SuperKlassCADyyYaFTV'
  @concurrent override func callerTest() async {}
  @concurrent override func concurrentTest() async {}
  @concurrent override func mainActorTest() async {}
}

class AllNonIsolatedUnsafe : SuperKlass {
  override nonisolated(nonsending) func callerTest() async {}

  // CHECK-LABEL: // vtable thunk for SuperKlass.concurrentTest() dispatching to AllNonIsolatedUnsafe.concurrentTest()
  // CHECK-NEXT: sil private [thunk] [ossa] @$s21attr_execution_silgen20AllNonIsolatedUnsafeC14concurrentTestyyYaFAA10SuperKlassCADyyYaFTV : $@convention(method) @async (@guaranteed AllNonIsolatedUnsafe) -> () {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed
  // CHECK:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen20AllNonIsolatedUnsafeC14concurrentTestyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed AllNonIsolatedUnsafe) -> ()
  // CHECK:   apply [[FUNC]]([[ACTOR]], [[ARG]])
  // CHECK: } // end sil function '$s21attr_execution_silgen20AllNonIsolatedUnsafeC14concurrentTestyyYaFAA10SuperKlassCADyyYaFTV'
  override nonisolated(nonsending) func concurrentTest() async {}

  // CHECK-LABEL: // vtable thunk for SuperKlass.mainActorTest() dispatching to AllNonIsolatedUnsafe.mainActorTest()
  // CHECK-NEXT: sil private [thunk] [ossa] @$s21attr_execution_silgen20AllNonIsolatedUnsafeC13mainActorTestyyYaFAA10SuperKlassCADyyYaFTV : $@convention(method) @async (@guaranteed AllNonIsolatedUnsafe) -> () {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $AllNonIsolatedUnsafe):
  // CHECK:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
  // CHECK:   [[ACTOR_E:%.*]] = init_existential_ref [[ACTOR]]
  // CHECK:   [[ACTOR_E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[ACTOR_E]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen20AllNonIsolatedUnsafeC13mainActorTestyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed AllNonIsolatedUnsafe) -> ()
  // CHECK:   apply [[FUNC]]([[ACTOR_E_OPT]], [[ARG]])
  // CHECK: } // end sil function '$s21attr_execution_silgen20AllNonIsolatedUnsafeC13mainActorTestyyYaFAA10SuperKlassCADyyYaFTV'
  override nonisolated(nonsending) func mainActorTest() async {}
}

// CHECK-LABEL: sil_vtable AllConcurrent {
// CHECK-NEXT:   #SuperKlass.callerTest: (SuperKlass) -> () async -> () : @$s21attr_execution_silgen13AllConcurrentC10callerTestyyYaFAA10SuperKlassCADyyYaFTV [override]	// vtable thunk for SuperKlass.callerTest() dispatching to AllConcurrent.callerTest()
// CHECK-NEXT:   #SuperKlass.concurrentTest: (SuperKlass) -> () async -> () : @$s21attr_execution_silgen13AllConcurrentC14concurrentTestyyYaF [override]	// AllConcurrent.concurrentTest()
// CHECK-NEXT:   #SuperKlass.mainActorTest: (SuperKlass) -> () async -> () : @$s21attr_execution_silgen13AllConcurrentC13mainActorTestyyYaF [override]	// AllConcurrent.mainActorTest()
// CHECK-NEXT:   #SuperKlass.init!allocator: (SuperKlass.Type) -> () -> SuperKlass : @$s21attr_execution_silgen13AllConcurrentCACycfC [override]	// AllConcurrent.__allocating_init()
// CHECK-NEXT:   #AllConcurrent.deinit!deallocator: @$s21attr_execution_silgen13AllConcurrentCfD	// AllConcurrent.__deallocating_deinit
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable AllNonIsolatedUnsafe {
// CHECK-NEXT:   #SuperKlass.callerTest: (SuperKlass) -> () async -> () : @$s21attr_execution_silgen20AllNonIsolatedUnsafeC10callerTestyyYaF [override]	// AllNonIsolatedUnsafe.callerTest()
// CHECK-NEXT:   #SuperKlass.concurrentTest: (SuperKlass) -> () async -> () : @$s21attr_execution_silgen20AllNonIsolatedUnsafeC14concurrentTestyyYaFAA10SuperKlassCADyyYaFTV [override]	// vtable thunk for SuperKlass.concurrentTest() dispatching to AllNonIsolatedUnsafe.concurrentTest()
// CHECK-NEXT:   #SuperKlass.mainActorTest: (SuperKlass) -> () async -> () : @$s21attr_execution_silgen20AllNonIsolatedUnsafeC13mainActorTestyyYaFAA10SuperKlassCADyyYaFTV [override]	// vtable thunk for SuperKlass.mainActorTest() dispatching to AllNonIsolatedUnsafe.mainActorTest()
// CHECK-NEXT:   #SuperKlass.init!allocator: (SuperKlass.Type) -> () -> SuperKlass : @$s21attr_execution_silgen20AllNonIsolatedUnsafeCACycfC [override]	// AllNonIsolatedUnsafe.__allocating_init()
// CHECK-NEXT:   #AllNonIsolatedUnsafe.deinit!deallocator: @$s21attr_execution_silgen20AllNonIsolatedUnsafeCfD	// AllNonIsolatedUnsafe.__deallocating_deinit
// CHECK-NEXT: }
