// RUN: %target-swift-emit-silgen %s -module-name attr_execution_silgen -target %target-swift-5.1-abi-triple -DSWIFT_FIVE | %FileCheck -check-prefix CHECK -check-prefix FIVE %s
// RUN: %target-swift-emit-silgen %s -swift-version 6 -module-name attr_execution_silgen -target %target-swift-5.1-abi-triple | %FileCheck -check-prefix CHECK -check-prefix SIX %s

// We codegen slightly differently for swift 5 vs swift 6, so we need to check
// both.

// REQUIRES: asserts
// REQUIRES: concurrency

protocol P {
  nonisolated(nonsending) func callerTest() async
  @concurrent func concurrentTest() async
  @MainActor func mainActorTest() async
}

protocol Q {
  nonisolated(nonsending) var test: String { get async throws }
  nonisolated(nonsending) func fnTest() async
}

struct AllDefault : P {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen10AllDefaultVAA1PA2aDP10callerTestyyYaFTW : $@convention(witness_method: P) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed AllDefault) -> () {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] : $*AllDefault):
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen10AllDefaultV10callerTestyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, AllDefault) -> ()
  // CHECK:   apply [[FUNC]]([[ACTOR]], [[LOAD]])
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$s21attr_execution_silgen10AllDefaultVAA1PA2aDP10callerTestyyYaFTW'
  func callerTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen10AllDefaultVAA1PA2aDP14concurrentTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllDefault) -> () {
  // CHECK: bb0([[SELF:%.*]] : $*AllDefault):
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen10AllDefaultV14concurrentTestyyYaF : $@convention(method) @async (AllDefault) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen10AllDefaultVAA1PA2aDP14concurrentTestyyYaFTW'
  func concurrentTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen10AllDefaultVAA1PA2aDP13mainActorTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllDefault) -> () {
  // CHECK: bb0([[SELF:%.*]] : $*AllDefault):
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen10AllDefaultV13mainActorTestyyYaF : $@convention(method) @async (AllDefault) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen10AllDefaultVAA1PA2aDP13mainActorTestyyYaFTW'
  func mainActorTest() async {}
}

struct AllCaller : P {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen9AllCallerVAA1PA2aDP10callerTestyyYaFTW : $@convention(witness_method: P) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed AllCaller) -> () {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] : $*AllCaller):
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen9AllCallerV10callerTestyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, AllCaller) -> ()
  // CHECK:   apply [[FUNC]]([[ACTOR]], [[LOAD]])
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$s21attr_execution_silgen9AllCallerVAA1PA2aDP10callerTestyyYaFTW'
  nonisolated(nonsending) func callerTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen9AllCallerVAA1PA2aDP14concurrentTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllCaller) -> () {
  // CHECK: bb0([[SELF:%.*]] : $*AllCaller):
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen9AllCallerV14concurrentTestyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, AllCaller) -> ()
  // CHECK:   [[NIL:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
  // CHECK:   hop_to_executor [[NIL]]
  // CHECK:   apply [[FUNC]]([[NIL]], [[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen9AllCallerVAA1PA2aDP14concurrentTestyyYaFTW'
  nonisolated(nonsending) func concurrentTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen9AllCallerVAA1PA2aDP13mainActorTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllCaller) -> () {
  // CHECK: bb0([[SELF:%.*]] : $*AllCaller):
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen9AllCallerV13mainActorTestyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, AllCaller) -> ()
  // CHECK:   [[MAIN_ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
  // CHECK:   [[EXIS_MAIN_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR]]
  // CHECK:   [[OPT_MAIN_ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[EXIS_MAIN_ACTOR]]
  // CHECK:   hop_to_executor [[OPT_MAIN_ACTOR]]
  // CHECK:   apply [[FUNC]]([[OPT_MAIN_ACTOR]], [[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen9AllCallerVAA1PA2aDP13mainActorTestyyYaFTW'
  nonisolated(nonsending) func mainActorTest() async {}
}

struct AllConcurrent : P {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP10callerTestyyYaFTW : $@convention(witness_method: P) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed AllConcurrent) -> () {
  // CHECK: bb0([[ACTOR]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] :
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen13AllConcurrentV10callerTestyyYaF : $@convention(method) @async (AllConcurrent) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP10callerTestyyYaFTW'
  @concurrent func callerTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP14concurrentTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllConcurrent) -> () {
  // CHECK: bb0([[SELF:%.*]] :  
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen13AllConcurrentV14concurrentTestyyYaF : $@convention(method) @async (AllConcurrent) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP14concurrentTestyyYaFTW'
  @concurrent func concurrentTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP13mainActorTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllConcurrent) -> () {
  // CHECK: bb0([[SELF:%.*]] :
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen13AllConcurrentV13mainActorTestyyYaF : $@convention(method) @async (AllConcurrent) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP13mainActorTestyyYaFTW'
  @concurrent func mainActorTest() async {}
}

struct AllMainActor : P {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen12AllMainActorVAA1PA2aDP10callerTestyyYaFTW : $@convention(witness_method: P) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed AllMainActor) -> () {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] :
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen12AllMainActorV10callerTestyyYaF : $@convention(method) @async (AllMainActor) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$s21attr_execution_silgen12AllMainActorVAA1PA2aDP10callerTestyyYaFTW'
  @MainActor func callerTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen12AllMainActorVAA1PA2aDP14concurrentTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllMainActor) -> () {
  // CHECK: bb0([[SELF:%.*]] :
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen12AllMainActorV14concurrentTestyyYaF : $@convention(method) @async (AllMainActor) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen12AllMainActorVAA1PA2aDP14concurrentTestyyYaFTW'
  @MainActor func concurrentTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen12AllMainActorVAA1PA2aDP04mainF4TestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllMainActor) -> () {
  // CHECK: bb0([[SELF:%.*]] :
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen12AllMainActorV04mainF4TestyyYaF : $@convention(method) @async (AllMainActor) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen12AllMainActorVAA1PA2aDP04mainF4TestyyYaFTW'
  @MainActor func mainActorTest() async {}
}

// Make sure that stored/non-async witness doesn't inherit `nonisolated(nonsending)` but thunk dismatches correctly

struct TestWitnessWithStorage: Q {
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s21attr_execution_silgen22TestWitnessWithStorageV4testSSvg : $@convention(method) (@guaranteed TestWitnessWithStorage) -> @owned String
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s21attr_execution_silgen22TestWitnessWithStorageV4testSSvs : $@convention(method) (@owned String, @inout TestWitnessWithStorage) -> ()
  var test: String

  // CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen22TestWitnessWithStorageV02fnD0yyF : $@convention(method) (@guaranteed TestWitnessWithStorage) -> ()
  func fnTest() {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen22TestWitnessWithStorageVAA1QA2aDP4testSSvgTW : $@convention(witness_method: Q) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed TestWitnessWithStorage) -> (@owned String, @error any Error)
  // CHECK: bb0([[ISOLATION:%.*]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] : $*TestWitnessWithStorage):
  // CHECK-NEXT: [[BORROWED_SELF:%.*]] = load_borrow [[SELF]]
  // CHECK: [[WITNESS:%.*]] = function_ref @$s21attr_execution_silgen22TestWitnessWithStorageV4testSSvg : $@convention(method) (@guaranteed TestWitnessWithStorage) -> @owned String
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[WITNESS]]([[BORROWED_SELF]]) : $@convention(method) (@guaranteed TestWitnessWithStorage) -> @owned String
  // CHECK-NEXT: hop_to_executor [[ISOLATION]]
  // CHECK-NEXT: end_borrow [[BORROWED_SELF]]
  // CHECK-NEXT: return [[RESULT]]
  // CHECK-NEXT: } // end sil function '$s21attr_execution_silgen22TestWitnessWithStorageVAA1QA2aDP4testSSvgTW'

  // CHECK: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen22TestWitnessWithStorageVAA1QA2aDP02fnD0yyYaFTW : $@convention(witness_method: Q) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed TestWitnessWithStorage) -> ()
  // CHECK: bb0([[ISOLATION:%.*]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] : $*TestWitnessWithStorage):
  // CHECK-NEXT: [[BORROWED_SELF]] = load_borrow [[SELF]]
  // CHECK: [[WITNESS:%.*]] = function_ref @$s21attr_execution_silgen22TestWitnessWithStorageV02fnD0yyF : $@convention(method) (@guaranteed TestWitnessWithStorage) -> ()
  // CHECK-NEXT: {{.*}} = apply [[WITNESS]]([[BORROWED_SELF]]) : $@convention(method) (@guaranteed TestWitnessWithStorage) -> ()
  // CHECK-NEXT: [[RESULT:%.*]] = tuple ()
  // CHECK-NEXT: hop_to_executor [[ISOLATION]]
  // CHECK-NEXT: end_borrow [[BORROWED_SELF]]
  // CHECK-NEXT: return [[RESULT]]
  // CHECK-NEXT: } // end sil function '$s21attr_execution_silgen22TestWitnessWithStorageVAA1QA2aDP02fnD0yyYaFTW'
}

struct TestSyncWitness: Q {
  // CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen15TestSyncWitnessV4testSSvg : $@convention(method) (TestSyncWitness) -> @owned String
  var test: String { "" }

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen15TestSyncWitnessVAA1QA2aDP4testSSvgTW : $@convention(witness_method: Q) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed TestSyncWitness) -> (@owned String, @error any Error)
  // CHECK: bb0([[ISOLATION:%.*]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] : $*TestSyncWitness):
  // CHECK-NEXT: [[BORROWED_SELF:%.*]] = load [trivial] [[SELF]]
  // CHECK: [[WITNESS:%.*]] = function_ref @$s21attr_execution_silgen15TestSyncWitnessV4testSSvg : $@convention(method) (TestSyncWitness) -> @owned String
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[WITNESS]]([[BORROWED_SELF]]) : $@convention(method) (TestSyncWitness) -> @owned String
  // CHECK-NEXT: hop_to_executor [[ISOLATION]]
  // CHECK-NEXT: return [[RESULT]]
  // CHECK-NEXT: } // end sil function '$s21attr_execution_silgen15TestSyncWitnessVAA1QA2aDP4testSSvgTW'

  // Tested in the `TestWitnessWithStorage`
  func fnTest() {}
}
