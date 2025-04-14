// RUN: %target-swift-emit-silgen %s -module-name attr_execution_silgen -target %target-swift-5.1-abi-triple -enable-experimental-feature ExecutionAttribute -DSWIFT_FIVE | %FileCheck -check-prefix CHECK -check-prefix FIVE %s
// RUN: %target-swift-emit-silgen %s -swift-version 6 -module-name attr_execution_silgen -target %target-swift-5.1-abi-triple -enable-experimental-feature ExecutionAttribute | %FileCheck -check-prefix CHECK -check-prefix SIX %s

// We codegen slightly differently for swift 5 vs swift 6, so we need to check
// both.

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: swift_feature_ExecutionAttribute

protocol P {
  @execution(caller) func callerTest() async
  @execution(concurrent) func concurrentTest() async
  @MainActor func mainActorTest() async
}

struct AllDefault : P {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen10AllDefaultVAA1PA2aDP10callerTestyyYaFTW : $@convention(witness_method: P) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed AllDefault) -> () {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] : $*AllDefault):
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen10AllDefaultV10callerTestyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, AllDefault) -> ()
  // CHECK:   apply [[FUNC]]([[ACTOR]], [[LOAD]])
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
  // CHECK: } // end sil function '$s21attr_execution_silgen9AllCallerVAA1PA2aDP10callerTestyyYaFTW'
  @execution(caller) func callerTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen9AllCallerVAA1PA2aDP14concurrentTestyyYaFTW : $@convention(witness_method: P) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed AllCaller) -> () {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] : $*AllCaller):
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen9AllCallerV14concurrentTestyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, AllCaller) -> ()
  // CHECK:   [[NIL:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
  // CHECK:   apply [[FUNC]]([[NIL]], [[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen9AllCallerVAA1PA2aDP14concurrentTestyyYaFTW'
  @execution(caller) func concurrentTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen9AllCallerVAA1PA2aDP13mainActorTestyyYaFTW : $@convention(witness_method: P) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed AllCaller) -> () {
  // CHECK: bb0({{%.*}} : @guaranteed $Optional<any Actor>, [[SELF:%.*]] : $*AllCaller):
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen9AllCallerV13mainActorTestyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, AllCaller) -> ()
  // CHECK:   [[MAIN_ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
  // CHECK:   [[EXIS_MAIN_ACTOR:%.*]] = init_existential_ref [[MAIN_ACTOR]]
  // CHECK:   [[OPT_MAIN_ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[EXIS_MAIN_ACTOR]]
  // CHECK:   apply [[FUNC]]([[OPT_MAIN_ACTOR]], [[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen9AllCallerVAA1PA2aDP13mainActorTestyyYaFTW'
  @execution(caller) func mainActorTest() async {}
}

struct AllConcurrent : P {
  // TODO: This seems wrong. We need to have our thunk have the implicit
  // isolated parameter from an ABI perspective.
  //
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP10callerTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllConcurrent) -> () {
  // CHECK: bb0([[SELF:%.*]] :
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen13AllConcurrentV10callerTestyyYaF : $@convention(method) @async (AllConcurrent) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP10callerTestyyYaFTW'
  @execution(concurrent) func callerTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP14concurrentTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllConcurrent) -> () {
  // CHECK: bb0([[SELF:%.*]] :  
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen13AllConcurrentV14concurrentTestyyYaF : $@convention(method) @async (AllConcurrent) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP14concurrentTestyyYaFTW'
  @execution(concurrent) func concurrentTest() async {}

  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP13mainActorTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllConcurrent) -> () {
  // CHECK: bb0([[SELF:%.*]] :
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen13AllConcurrentV13mainActorTestyyYaF : $@convention(method) @async (AllConcurrent) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
  // CHECK: } // end sil function '$s21attr_execution_silgen13AllConcurrentVAA1PA2aDP13mainActorTestyyYaFTW'
  @execution(concurrent) func mainActorTest() async {}
}

struct AllMainActor : P {
  // TODO: This is incorrect from an ABI perspective. The witness needs to have
  // the implicit isolated parameter.
  //
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21attr_execution_silgen12AllMainActorVAA1PA2aDP10callerTestyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed AllMainActor) -> () {
  // CHECK: bb0([[SELF:%.*]] :
  // CHECK:   [[LOAD:%.*]] = load [trivial] [[SELF]]
  // CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen12AllMainActorV10callerTestyyYaF : $@convention(method) @async (AllMainActor) -> ()
  // CHECK:   apply [[FUNC]]([[LOAD]])
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
