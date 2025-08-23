// RUN: %target-swift-emit-silgen %s -module-name attr_execution_silgen -target %target-swift-5.1-abi-triple -DSWIFT_FIVE | %FileCheck -check-prefix CHECK -check-prefix FIVE %s
// RUN: %target-swift-emit-silgen %s -swift-version 6 -module-name attr_execution_silgen -target %target-swift-5.1-abi-triple | %FileCheck -check-prefix CHECK -check-prefix SIX %s

// We codegen slightly differently for swift 5 vs swift 6, so we need to check
// both.

// REQUIRES: asserts
// REQUIRES: concurrency

////////////////////////
// MARK: Declarations //
////////////////////////

nonisolated(nonsending)
func globalCallerFunc() async -> () {}

@concurrent
func globalConcurrentFunc() async -> () {}

class NonSendableKlass {
  init() {}
}
class SendableKlass : @unchecked Sendable {
  init() {}
}

nonisolated(nonsending)
func globalCallerFuncSendableKlass(_ x: SendableKlass) async -> () {}

@concurrent
func globalConcurrentFuncSendableKlass(_ x: SendableKlass) async -> () {}

nonisolated(nonsending)
func globalCallerFuncSendableKlass(_ x: SendableKlass) async -> SendableKlass { fatalError() }

@concurrent
func globalConcurrentFuncSendableKlass(_ x: SendableKlass) async -> SendableKlass { fatalError() }


/////////////////
// MARK: Tests //
/////////////////

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen33testCallerToConcurrentNonIsolatedyyyyYaYCcYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()):
// CHECK:   [[FUNC_COPY:%.*]] = copy_value [[FUNC]]
// CHECK:   [[THUNK:%.*]] = function_ref @$sScA_pSgIegHgIL_IegH_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> ()
// CHECK:   partial_apply [callee_guaranteed] [[THUNK]]([[FUNC_COPY]])
// CHECK: } // end sil function '$s21attr_execution_silgen33testCallerToConcurrentNonIsolatedyyyyYaYCcYaF'
public func testCallerToConcurrentNonIsolated(_ x: nonisolated(nonsending) @escaping () async -> ()) async {
  let y: @concurrent () async -> () = x
  await y()
}

// This thunk is used to convert a caller to a concurrent function. So, we pass in .none as the
// isolated parameter.
//
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sScA_pSgIegHgIL_IegH_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()):
// CHECK:   [[ENUM:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK:   hop_to_executor [[ENUM]]
// CHECK:   apply [[FUNC]]([[ENUM]])
// CHECK: } // end sil function '$sScA_pSgIegHgIL_IegH_TR'

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen31testCallerToConcurrentMainActoryyyyYaYCcYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()):
// CHECK:   [[FUNC_COPY:%.*]] = copy_value [[FUNC]]
// CHECK:   [[THUNK:%.*]] = function_ref @$sScA_pSgIegHgIL_IegH_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> ()
// CHECK:   partial_apply [callee_guaranteed] [[THUNK]]([[FUNC_COPY]])
// CHECK: } // end sil function '$s21attr_execution_silgen31testCallerToConcurrentMainActoryyyyYaYCcYaF'
@MainActor
public func testCallerToConcurrentMainActor(_ x: nonisolated(nonsending) @escaping () async -> ()) async {
  let y: @concurrent () async -> () = x
  await y()
}

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen33testConcurrentToCallerNonIsolatedyyyyYacYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed () -> ()):
// CHECK:   [[FUNC_COPY:%.*]] = copy_value [[FUNC]]
// CHECK:   [[THUNK:%.*]] = function_ref @$sIegH_ScA_pSgIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   partial_apply [callee_guaranteed] [[THUNK]]([[FUNC_COPY]])
// CHECK: } // end sil function '$s21attr_execution_silgen33testConcurrentToCallerNonIsolatedyyyyYacYaF'
public func testConcurrentToCallerNonIsolated(_ x: @escaping @concurrent () async -> ()) async {
  let y: nonisolated(nonsending) () async -> () = x
  await y()
}

// This thunk has the "surface" of an @caller function, but just ignores the
// isolated parameter and calls the concurrent funciton.
//
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIegH_ScA_pSgIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @async @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed () -> ()):
// CHECK:   apply [[FUNC]]
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$sIegH_ScA_pSgIegHgIL_TR'

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen42testConcurrentToCallerNonIsolatedMainActoryyyyYacYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] :
// CHECK:   [[FUNC_COPY:%.*]] = copy_value [[FUNC]]
// CHECK:   [[THUNK:%.*]] = function_ref @$sIegH_ScA_pSgIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   partial_apply [callee_guaranteed] [[THUNK]]([[FUNC_COPY]])
// CHECK: } // end sil function '$s21attr_execution_silgen42testConcurrentToCallerNonIsolatedMainActoryyyyYacYaF'
@MainActor
public func testConcurrentToCallerNonIsolatedMainActor(_ x: @escaping @concurrent () async -> ()) async {
  let y: nonisolated(nonsending) () async -> () = x
  await y()
}

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen016testConcurrentToE0yyyyYacYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed () -> ()):
// CHECK:   [[COPY:%.*]] = copy_value [[FUNC]]
// CHECK:   [[Y:%.*]] = move_value [lexical] [var_decl] [[COPY]]
// CHECK:   [[BORROW_Y:%.*]] = begin_borrow [[Y]]
// CHECK:   [[COPY_Y:%.*]] = copy_value [[BORROW_Y]]
// CHECK:   [[BORROW_COPY_Y:%.*]] = begin_borrow [[COPY_Y]]
// CHECK:   apply [[BORROW_COPY_Y]]()
// CHECK:   [[FUNC:%.*]] = function_ref @$s21attr_execution_silgen20globalConcurrentFuncyyYaF : $@convention(thin) @async () -> ()
// CHECK:   [[TTFI:%.*]] = thin_to_thick_function [[FUNC]]
// FIVE:    [[Z:%.*]] = move_value [lexical] [var_decl] [[TTFI]]
// SIX:     [[CVT_1:%.*]] = convert_function [[TTFI]] to $@Sendable @async
// SIX:     [[CVT_2:%.*]] = convert_function [[CVT_1]] to $@async
// SIX:     [[Z:%.*]] = move_value [lexical] [var_decl] [[CVT_2]]
// CHECK:   [[BORROW_Z:%.*]] = begin_borrow [[Z]]
// CHECK:   [[COPY_Z:%.*]] = copy_value [[BORROW_Z]]
// CHECK:   [[BORROW_COPY_Z:%.*]] = begin_borrow [[COPY_Z]]
// CHECK:   apply [[BORROW_COPY_Z]]()
// CHECK: } // end sil function '$s21attr_execution_silgen016testConcurrentToE0yyyyYacYaF'
public func testConcurrentToConcurrent(_ x: @escaping @concurrent () async -> ()) async {
  let y: @concurrent () async -> () = x
  await y()
  let z: @concurrent () async -> () = globalConcurrentFunc
  await z()
}

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen012testCallerToE0yyyyYaYCcYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()):
// CHECK:   [[COPY:%.*]] = copy_value [[FUNC]]
// CHECK:   [[Y:%.*]] = move_value [lexical] [var_decl] [[COPY]]
// CHECK:   [[BORROW_Y:%.*]] = begin_borrow [[Y]]
// CHECK:   [[COPY_Y:%.*]] = copy_value [[BORROW_Y]]
// CHECK:   [[BORROW_COPY_Y:%.*]] = begin_borrow [[COPY_Y]]
// CHECK:   apply [[BORROW_COPY_Y]]({{%.*}})
// CHECK: } // end sil function '$s21attr_execution_silgen012testCallerToE0yyyyYaYCcYaF'
//
// z has a round trip issue.
public func testCallerToCaller(_ x: nonisolated(nonsending) @escaping () async -> ()) async {
  let y: nonisolated(nonsending) () async -> () = x
  await y()
  let z: nonisolated(nonsending) () async -> () = globalCallerFunc
  await z()
}

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen24testCallerLocalVariablesyyyyYaYCcYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> () {
// CHECK: bb0([[PARAM:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()):
// CHECK:   [[PARAM_COPY:%.*]] = copy_value [[PARAM]]
// CHECK:   [[Y:%.*]] = move_value [lexical] [var_decl] [[PARAM_COPY]]
// CHECK:   [[Y_B:%.*]] = begin_borrow [[Y]]
// CHECK:   [[Y_B_C:%.*]] = copy_value [[Y_B]]
// CHECK:   [[Y2:%.*]] = move_value [lexical] [var_decl] [[Y_B_C]]
// CHECK:   [[Y2_B:%.*]] = begin_borrow [[Y2]]
// CHECK:   [[Y2_B_C:%.*]] = copy_value [[Y2_B]]
// CHECK:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK:   [[Y2_B_C_B:%.*]] = begin_borrow [[Y2_B_C]]
// CHECK:   apply [[Y2_B_C_B]]([[ACTOR]])
// CHECK: } // end sil function '$s21attr_execution_silgen24testCallerLocalVariablesyyyyYaYCcYaF'
public func testCallerLocalVariables(_ x: nonisolated(nonsending) @escaping () async -> ()) async {
  let y: nonisolated(nonsending) () async -> () = x
  let y2: nonisolated(nonsending) () async -> () = y
  await y2()
}

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen28testConcurrentLocalVariablesyyyyYacYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed () -> ()) -> () {
// CHECK:   [[PARAM_COPY:%.*]] = copy_value [[PARAM]]
// CHECK:   [[Y:%.*]] = move_value [lexical] [var_decl] [[PARAM_COPY]]
// CHECK:   [[Y_B:%.*]] = begin_borrow [[Y]]
// CHECK:   [[Y_B_C:%.*]] = copy_value [[Y_B]]
// CHECK:   [[Y2:%.*]] = move_value [lexical] [var_decl] [[Y_B_C]]
// CHECK:   [[Y2_B:%.*]] = begin_borrow [[Y2]]
// CHECK:   [[Y2_B_C:%.*]] = copy_value [[Y2_B]]
// CHECK:   [[Y2_B_C_B:%.*]] = begin_borrow [[Y2_B_C]]
// CHECK:   apply [[Y2_B_C_B]]()
// CHECK: } // end sil function '$s21attr_execution_silgen28testConcurrentLocalVariablesyyyyYacYaF'
public func testConcurrentLocalVariables(_ x: @escaping @concurrent () async -> ()) async {
  let y: @concurrent () async -> () = x
  let y2: @concurrent () async -> () = y
  await y2()
}

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen34testCallerConcurrentLocalVariablesyyyyYaYCcYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> () {
// CHECK: bb0([[PARAM:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()):
// CHECK:   [[PARAM_COPY:%.*]] = copy_value [[PARAM]]
// CHECK:   [[THUNK_1:%.*]] = function_ref @$sScA_pSgIegHgIL_IegH_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> ()
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK_1]]([[PARAM_COPY]])
// CHECK:   [[Y:%.*]] = move_value [lexical] [var_decl] [[PA]]
// CHECK:   [[Y_B:%.*]] = begin_borrow [[Y]]
// CHECK:   [[Y_B_C:%.*]] = copy_value [[Y_B]]
// CHECK:   [[THUNK_2:%.*]] = function_ref @$sIegH_ScA_pSgIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   [[PA_2:%.*]] = partial_apply [callee_guaranteed] [[THUNK_2]]([[Y_B_C]])
// CHECK: } // end sil function '$s21attr_execution_silgen34testCallerConcurrentLocalVariablesyyyyYaYCcYaF'
public func testCallerConcurrentLocalVariables(_ x: nonisolated(nonsending) @escaping () async -> ()) async {
  let y: @concurrent () async -> () = x
  let y2: nonisolated(nonsending) () async -> () = y
  await y2()
}

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen34testConcurrentCallerLocalVariablesyyyyYacYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[PARAM:%.*]] : @guaranteed $@async @callee_guaranteed () -> ()):
// CHECK:   [[PARAM_C:%.*]] = copy_value [[PARAM]]
// CHECK:   [[THUNK_1:%.*]] = function_ref @$sIegH_ScA_pSgIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK_1]]([[PARAM_C]])
// CHECK:   [[Y:%.*]] = move_value [lexical] [var_decl] [[PA]]
// CHECK:   [[Y_B:%.*]] = begin_borrow [[Y]]
// CHECK:   [[Y_B_C:%.*]] = copy_value [[Y_B]]
// CHECK:   [[THUNK_2:%.*]] = function_ref @$sScA_pSgIegHgIL_IegH_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> ()
// CHECK:   [[PA_2:%.*]] = partial_apply [callee_guaranteed] [[THUNK_2]]([[Y_B_C]])
// CHECK: } // end sil function '$s21attr_execution_silgen34testConcurrentCallerLocalVariablesyyyyYacYaF'
public func testConcurrentCallerLocalVariables(_ x: @escaping @concurrent () async -> ()) async {
  let y: nonisolated(nonsending) () async -> () = x
  let y2: @concurrent () async -> () = y
  await y2()
}

// CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen22globalActorConversionsyyyyYac_yyYaYCctYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed () -> (), @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> () {
// CHECK: bb0([[X:%.*]] : @guaranteed $@async @callee_guaranteed () -> (), [[Y:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()):
// CHECK:   [[GLOBAL_CALLER_FUNC:%.*]] = function_ref @$s21attr_execution_silgen16globalCallerFuncyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
// FIVE:    [[TTFI:%.*]] = thin_to_thick_function [[GLOBAL_CALLER_FUNC]] to $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
// FIVE:    [[THUNK:%.*]] = function_ref @$sScA_pSgIegHgIL_IegH_TRScMTU : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> ()
// FIVE:    [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[TTFI]]) :
// SIX:     [[THUNK:%.*]] = function_ref @$sScA_pSgIetHgIL_IeghH_TRScMTU : $@convention(thin) @Sendable @async (@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> ()
// SIX:     [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[GLOBAL_CALLER_FUNC]]) :
// CHECK:   [[V1:%.*]] = move_value [lexical] [var_decl] [[PA]]
// CHECK:   [[V1_B:%.*]] = begin_borrow [[V1]]
// CHECK:   [[V1_B_C:%.*]] = copy_value [[V1_B]]
// CHECK:   [[V1_B_C_B:%.*]] = begin_borrow [[V1_B_C]]
// FIVE:    apply [[V1_B_C_B]]() : $@async @callee_guaranteed () -> ()
// SIX:     apply [[V1_B_C_B]]() : $@Sendable @async @callee_guaranteed () -> ()

// CHECK:   [[GLOBAL_CONCURRENT_FUNC:%.*]] = function_ref @$s21attr_execution_silgen20globalConcurrentFuncyyYaF : $@convention(thin) @async () -> ()
// CHECK:   [[TTFI:%.*]] = thin_to_thick_function [[GLOBAL_CONCURRENT_FUNC]]
// FIVE:    [[V2:%.*]] = move_value [lexical] [var_decl] [[TTFI]]
// SIX:     [[CVT:%.*]] = convert_function [[TTFI]] to $@Sendable @async @callee_guaranteed () -> ()
// SIX:     [[V2:%.*]] = move_value [lexical] [var_decl] [[CVT]]
// CHECK:   [[V2_B:%.*]] = begin_borrow [[V2]]
// CHECK:   [[V2_B_C:%.*]] = copy_value [[V2_B]]
// CHECK:   [[V2_B_C_B:%.*]] = begin_borrow [[V2_B_C]]
// CHECK:   apply [[V2_B_C_B]]()

// FIVE:   [[X_C:%.*]] = copy_value [[X]]
// FIVE:   [[V3:%.*]] = move_value [lexical] [var_decl] [[X_C]]
// FIVE:   [[V3_B:%.*]] = begin_borrow [[V3]]
// FIVE:   [[V3_B_C:%.*]] = copy_value [[V3_B]]
// FIVE:   [[V3_B_C_B:%.*]] = begin_borrow [[V3_B_C]]
// FIVE:   apply [[V3_B_C_B]]()

// FIVE:   [[Y_C:%.*]] = copy_value [[Y]]
// FIVE:   [[THUNK:%.*]] = function_ref @$sScA_pSgIegHgIL_IegH_TRScMTU : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> ()
// FIVE:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])
// FIVE:   [[V4:%.*]] = move_value [lexical] [var_decl] [[PA]]
// FIVE:   [[V4_B:%.*]] = begin_borrow [[V4]]
// FIVE:   [[V4_B_C:%.*]] = copy_value [[V4_B]]
// FIVE:   [[V4_B_C_B:%.*]] = begin_borrow [[V4_B_C]]
// FIVE:   apply [[V4_B_C_B]]()

// CHECK: } // end sil function '$s21attr_execution_silgen22globalActorConversionsyyyyYac_yyYaYCctYaF'

// SIX-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sScA_pSgIetHgIL_IeghH_TRScMTU : $@convention(thin) @Sendable @async (@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> () {
// SIX: bb0([[FUNC:%.*]] : $@convention(thin) @async (@sil_isolated
// SIX:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// SIX:   [[E:%.*]] = init_existential_ref [[ACTOR]] : $MainActor : $MainActor, $any Actor
// SIX:   [[E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[E]]
// SIX:   hop_to_executor [[E_OPT]]
// SIX:   apply [[FUNC]]([[E_OPT]]) : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
// SIX: } // end sil function '$sScA_pSgIetHgIL_IeghH_TRScMTU'

func globalActorConversions(_ x: @escaping @concurrent () async -> (),
                            _ y: nonisolated(nonsending) @escaping () async -> ()) async {
  let v1: @MainActor () async -> Void = globalCallerFunc
  await v1()
  let v2: @MainActor () async -> Void = globalConcurrentFunc
  await v2()

  // These are invalid in swift 6 since x is not Sendable and this @MainActor
  // is.
#if SWIFT_FIVE
  let v3: @MainActor () async -> Void = x
  await v3()
  let v4: @MainActor () async -> Void = y
  await v4()
#endif
}

// CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen23globalActorConversions2yyyAA13SendableKlassCYac_yADYaYCctYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@guaranteed SendableKlass) -> (), @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()) -> () {
// CHECK: bb0([[X:%.*]] : @guaranteed $@async @callee_guaranteed (@guaranteed SendableKlass) -> (), [[Y:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()):
// CHECK:   [[GLOBAL_CALLER_FUNC:%.*]] = function_ref @$s21attr_execution_silgen29globalCallerFuncSendableKlassyyAA0gH0CYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()
// FIVE:    [[TTFI:%.*]] = thin_to_thick_function [[GLOBAL_CALLER_FUNC]] to $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()
// FIVE:    [[THUNK:%.*]] = function_ref @$sScA_pSg21attr_execution_silgen13SendableKlassCIegHgILg_ADIegHg_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()) -> ()
// FIVE:    [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[TTFI]]) :
// SIX:     [[THUNK:%.*]] = function_ref @$sScA_pSg21attr_execution_silgen13SendableKlassCIetHgILg_ADIeghHg_TRScMTU : $@convention(thin) @Sendable @async (@guaranteed SendableKlass, @convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()) -> ()
// SIX:     [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[GLOBAL_CALLER_FUNC]]) :
// CHECK:   [[V1:%.*]] = move_value [lexical] [var_decl] [[PA]]
// CHECK:   [[V1_B:%.*]] = begin_borrow [[V1]]
// CHECK:   [[V1_B_C:%.*]] = copy_value [[V1_B]]
// CHECK:   [[V1_B_C_B:%.*]] = begin_borrow [[V1_B_C]]
// FIVE:    apply [[V1_B_C_B]]({{%.*}}) : $@async @callee_guaranteed (@guaranteed SendableKlass) -> ()
// SIX:     apply [[V1_B_C_B]]({{%.*}}) : $@Sendable @async @callee_guaranteed (@guaranteed SendableKlass) -> ()

// CHECK:   [[GLOBAL_CONCURRENT_FUNC:%.*]] = function_ref @$s21attr_execution_silgen33globalConcurrentFuncSendableKlassyyAA0gH0CYaF : $@convention(thin) @async (@guaranteed SendableKlass) -> ()
// CHECK:   [[TTFI:%.*]] = thin_to_thick_function [[GLOBAL_CONCURRENT_FUNC]] to $@async @callee_guaranteed (@guaranteed SendableKlass) -> ()
// FIVE:    [[V2:%.*]] = move_value [lexical] [var_decl] [[TTFI]]
// SIX:     [[CVT:%.*]] = convert_function [[TTFI]] to $@Sendable @async @callee_guaranteed (@guaranteed SendableKlass) -> ()
// SIX:     [[V2:%.*]] = move_value [lexical] [var_decl] [[CVT]]
// CHECK:   [[V2_B:%.*]] = begin_borrow [[V2]]
// CHECK:   [[V2_B_C:%.*]] = copy_value [[V2_B]]
// CHECK:   [[V2_B_C_B:%.*]] = begin_borrow [[V2_B_C]]
// CHECK:   apply [[V2_B_C_B]]({{%.*}})

// FIVE:   [[X_C:%.*]] = copy_value [[X]]
// FIVE:   [[V3:%.*]] = move_value [lexical] [var_decl] [[X_C]]
// FIVE:   [[V3_B:%.*]] = begin_borrow [[V3]]
// FIVE:   [[V3_B_C:%.*]] = copy_value [[V3_B]]
// FIVE:   [[V3_B_C_B:%.*]] = begin_borrow [[V3_B_C]]
// FIVE:   apply [[V3_B_C_B]]({{%.*}})

// FIVE:   [[Y_C:%.*]] = copy_value [[Y]]
// FIVE:   [[THUNK:%.*]] = function_ref @$sScA_pSg21attr_execution_silgen13SendableKlassCIegHgILg_ADIegHg_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()) -> ()
// FIVE:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])
// FIVE:   [[V4:%.*]] = move_value [lexical] [var_decl] [[PA]]
// FIVE:   [[V4_B:%.*]] = begin_borrow [[V4]]
// FIVE:   [[V4_B_C:%.*]] = copy_value [[V4_B]]
// FIVE:   [[V4_B_C_B:%.*]] = begin_borrow [[V4_B_C]]
// FIVE:   apply [[V4_B_C_B]]({{%.*}})

// CHECK: } // end sil function '$s21attr_execution_silgen23globalActorConversions2yyyAA13SendableKlassCYac_yADYaYCctYaF'

// FIVE-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sScA_pSg21attr_execution_silgen13SendableKlassCIegHgILg_ADIegHg_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()) -> () {
// FIVE: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guaranteed
// FIVE:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// FIVE:   [[E:%.*]] = init_existential_ref [[ACTOR]] : $MainActor : $MainActor, $any Actor
// FIVE:   [[E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[E]]
// FIVE:   hop_to_executor [[E_OPT]]
// FIVE:   apply [[FUNC]]([[E_OPT]], [[ARG]])
// FIVE: } // end sil function '$sScA_pSg21attr_execution_silgen13SendableKlassCIegHgILg_ADIegHg_TRScMTU

// SIX-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sScA_pSg21attr_execution_silgen13SendableKlassCIetHgILg_ADIeghHg_TRScMTU : $@convention(thin) @Sendable @async (@guaranteed SendableKlass, @convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()) -> () {
// SIX: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()):
// SIX:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// SIX:   [[E:%.*]] = init_existential_ref [[ACTOR]] : $MainActor : $MainActor, $any Actor
// SIX:   [[E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[E]]
// SIX:   hop_to_executor [[E_OPT]]
// SIX:   apply [[FUNC]]([[E_OPT]], [[ARG]])
// SIX: // end sil function '$sScA_pSg21attr_execution_silgen13SendableKlassCIetHgILg_ADIeghHg_TRScMTU'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sScA_pSg21attr_execution_silgen13SendableKlassCIegHgILg_ADIegHg_TR : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guaranteed
// CHECK:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   apply [[FUNC]]([[ACTOR]], [[ARG]])
// CHECK: } // end sil function '$sScA_pSg21attr_execution_silgen13SendableKlassCIegHgILg_ADIegHg_TR'
func globalActorConversions2(_ x: @escaping @concurrent (SendableKlass) async -> (),
                             _ y: nonisolated(nonsending) @escaping (SendableKlass) async -> ()) async {
  let v1: @MainActor (SendableKlass) async -> Void = globalCallerFuncSendableKlass
  await v1(SendableKlass())
  let v2: @MainActor (SendableKlass) async -> Void = globalConcurrentFuncSendableKlass
  await v2(SendableKlass())
#if SWIFT_FIVE
  let v3: @MainActor (SendableKlass) async -> Void = x
  await v3(SendableKlass())
  let v4: @MainActor (SendableKlass) async -> Void = y
  await v4(SendableKlass())
#endif
  let v5: @concurrent (SendableKlass) async -> Void = y
  await v5(SendableKlass())
}

// CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen23globalActorConversions3yyAA13SendableKlassCADYac_A2DYaYCctYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@guaranteed SendableKlass) -> @owned SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass) -> () {
// CHECK: bb0([[X:%.*]] : @guaranteed $@async @callee_guaranteed (@guaranteed SendableKlass) -> @owned SendableKlass, [[Y:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass):
// CHECK:   [[GLOBAL_CALLER_FUNC:%.*]] = function_ref @$s21attr_execution_silgen29globalCallerFuncSendableKlassyAA0gH0CADYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass
// FIVE:    [[TTFI:%.*]] = thin_to_thick_function [[GLOBAL_CALLER_FUNC]] to $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass
// FIVE:    [[THUNK:%.*]] = function_ref @$sScA_pSg21attr_execution_silgen13SendableKlassCADIegHgILgo_A2DIegHgo_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass
// FIVE:    [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[TTFI]])
// SIX:     [[THUNK:%.*]] = function_ref @$sScA_pSg21attr_execution_silgen13SendableKlassCADIetHgILgo_A2DIeghHgo_TRScMTU : $@convention(thin) @Sendable @async (@guaranteed SendableKlass, @convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass
// SIX:     [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[GLOBAL_CALLER_FUNC]])
// CHECK:   [[V1:%.*]] = move_value [lexical] [var_decl] [[PA]]
// CHECK:   [[V1_B:%.*]] = begin_borrow [[V1]]
// CHECK:   [[V1_B_C:%.*]] = copy_value [[V1_B]]
// CHECK:   [[V1_B_C_B:%.*]] = begin_borrow [[V1_B_C]]
// CHECK:   apply [[V1_B_C_B]]({{%.*}})

// CHECK:   [[GLOBAL_CONCURRENT_FUNC:%.*]] = function_ref @$s21attr_execution_silgen33globalConcurrentFuncSendableKlassyAA0gH0CADYaF : $@convention(thin) @async (@guaranteed SendableKlass) -> @owned SendableKlass
// CHECK:   [[TTFI:%.*]] = thin_to_thick_function [[GLOBAL_CONCURRENT_FUNC]] to $@async @callee_guaranteed (@guaranteed SendableKlass) -> @owned SendableKlass
// FIVE:    [[V2:%.*]] = move_value [lexical] [var_decl] [[TTFI]]
// SIX:     [[CVT:%.*]] = convert_function [[TTFI]] to $@Sendable @async @callee_guaranteed (@guaranteed SendableKlass) -> @owned SendableKlass
// SIX:     [[V2:%.*]] = move_value [lexical] [var_decl] [[CVT]]
// CHECK:   [[V2_B:%.*]] = begin_borrow [[V2]]
// CHECK:   [[V2_B_C:%.*]] = copy_value [[V2_B]]
// CHECK:   [[V2_B_C_B:%.*]] = begin_borrow [[V2_B_C]]
// CHECK:   apply [[V2_B_C_B]]({{%.*}})

// FIVE:   [[X_C:%.*]] = copy_value [[X]]
// FIVE:   [[V3:%.*]] = move_value [lexical] [var_decl] [[X_C]]
// FIVE:   [[V3_B:%.*]] = begin_borrow [[V3]]
// FIVE:   [[V3_B_C:%.*]] = copy_value [[V3_B]]
// FIVE:   [[V3_B_C_B:%.*]] = begin_borrow [[V3_B_C]]
// FIVE:   apply [[V3_B_C_B]]({{%.*}})

// FIVE:   [[Y_C:%.*]] = copy_value [[Y]]
// FIVE:   [[THUNK:%.*]] = function_ref @$sScA_pSg21attr_execution_silgen13SendableKlassCADIegHgILgo_A2DIegHgo_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass
// FIVE:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])
// FIVE:   [[V4:%.*]] = move_value [lexical] [var_decl] [[PA]]
// FIVE:   [[V4_B:%.*]] = begin_borrow [[V4]]
// FIVE:   [[V4_B_C:%.*]] = copy_value [[V4_B]]
// FIVE:   [[V4_B_C_B:%.*]] = begin_borrow [[V4_B_C]]
// FIVE:   apply [[V4_B_C_B]]({{%.*}})

// CHECK:  [[Y_C:%.*]] = copy_value [[Y]]
// CHECK:  [[THUNK:%.*]] = function_ref @$sScA_pSg21attr_execution_silgen13SendableKlassCADIegHgILgo_A2DIegHgo_TR : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass
// CHECK:  [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])
// CHECK:  [[V5:%.*]] = move_value [lexical] [var_decl] [[PA]]
// CHECK: } // end sil function '$s21attr_execution_silgen23globalActorConversions3yyAA13SendableKlassCADYac_A2DYaYCctYaF'

// FIVE-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sScA_pSg21attr_execution_silgen13SendableKlassCADIegHgILgo_A2DIegHgo_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass {
// FIVE: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guaranteed
// FIVE:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// FIVE:   [[E:%.*]] = init_existential_ref [[ACTOR]] : $MainActor : $MainActor, $any Actor
// FIVE:   [[E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[E]]
// FIVE:   hop_to_executor [[E_OPT]]
// FIVE:   apply [[FUNC]]([[E_OPT]], [[ARG]])
// FIVE: } // end sil function '$sScA_pSg21attr_execution_silgen13SendableKlassCADIegHgILgo_A2DIegHgo_TRScMTU'

// SIX-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sScA_pSg21attr_execution_silgen13SendableKlassCADIetHgILgo_A2DIeghHgo_TRScMTU : $@convention(thin) @Sendable @async (@guaranteed SendableKlass, @convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass {
// SIX: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass):
// SIX:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// SIX:   [[E:%.*]] = init_existential_ref [[ACTOR]] : $MainActor : $MainActor, $any Actor
// SIX:   [[E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[E]]
// SIX:   hop_to_executor [[E_OPT]]
// SIX:   apply [[FUNC]]([[E_OPT]], [[ARG]])
// SIX: } // end sil function '$sScA_pSg21attr_execution_silgen13SendableKlassCADIetHgILgo_A2DIeghHgo_TRScMTU'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sScA_pSg21attr_execution_silgen13SendableKlassCADIegHgILgo_A2DIegHgo_TR : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guarantee
// CHECK: [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK: hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$sScA_pSg21attr_execution_silgen13SendableKlassCADIegHgILgo_A2DIegHgo_TR'
func globalActorConversions3(_ x: @escaping @concurrent (SendableKlass) async -> SendableKlass,
                             _ y: nonisolated(nonsending) @escaping (SendableKlass) async -> SendableKlass) async {
  let v1: @MainActor (SendableKlass) async -> SendableKlass = globalCallerFuncSendableKlass
  _ = await v1(SendableKlass())
  let v2: @MainActor (SendableKlass) async -> SendableKlass = globalConcurrentFuncSendableKlass
  _ = await v2(SendableKlass())
#if SWIFT_FIVE
  let v3: @MainActor (SendableKlass) async -> SendableKlass = x
  _ = await v3(SendableKlass())
  let v4: @MainActor (SendableKlass) async -> SendableKlass = y
  _ = await v4(SendableKlass())
#endif
  let v5: @concurrent (SendableKlass) async -> SendableKlass = y
  _ = await v5(SendableKlass())
}

// CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen26conversionsFromSyncToAsyncyyyAA16NonSendableKlassCYbc_yAA0jK0CYbScMYcctYaF : $@convention(thin) @async (@guaranteed @Sendable @callee_guaranteed (@guaranteed NonSendableKlass) -> (), @guaranteed @Sendable @callee_guaranteed (@guaranteed SendableKlass) -> ()) -> ()
// CHECK: bb0([[X:%.*]] : @guaranteed $@Sendable @callee_guaranteed (@guaranteed NonSendableKlass) -> (), [[Y:%.*]] : @guaranteed $@Sendable @callee_guaranteed (@guaranteed SendableKlass) -> ()):

// CHECK:   [[X_C:%.*]] = copy_value [[X]]
// CHECK:   [[THUNK:%.*]] = function_ref @$s21attr_execution_silgen16NonSendableKlassCIeghg_ScA_pSgACIegHgILg_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed NonSendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed NonSendableKlass) -> ()) -> ()
// CHECK:   [[PAI:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[X_C]])

// CHECK:   [[Y_C:%.*]] = copy_value [[Y]]
// CHECK:   [[THUNK:%.*]] = function_ref @$s21attr_execution_silgen13SendableKlassCIeghg_ScA_pSgACIegHgILg_TRScMTU : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed SendableKlass) -> ()) -> ()
// CHECK:   [[PAI:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])

// CHECK:   [[Y_C:%.*]] = copy_value [[Y]]
// CHECK:   [[THUNK:%.*]] = function_ref @$s21attr_execution_silgen13SendableKlassCIeghg_ACIegHg_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed SendableKlass) -> ()) -> ()
// CHECK:   [[PAI:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])

// CHECK: } // end sil function '$s21attr_execution_silgen26conversionsFromSyncToAsyncyyyAA16NonSendableKlassCYbc_yAA0jK0CYbScMYcctYaF'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s21attr_execution_silgen16NonSendableKlassCIeghg_ScA_pSgACIegHgILg_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed NonSendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed NonSendableKlass) -> ()) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>
// CHECK: apply {{%.*}}({{%.*}}) : $@Sendable @callee_guaranteed (@guaranteed NonSendableKlass) -> ()
// CHECK: hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$s21attr_execution_silgen16NonSendableKlassCIeghg_ScA_pSgACIegHgILg_TR'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s21attr_execution_silgen13SendableKlassCIeghg_ScA_pSgACIegHgILg_TRScMTU : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed SendableKlass) -> ()) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guaranteed $@Sendable @callee_guaranteed
// CHECK:   [[MAIN_ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK:   [[MAIN_ACTOR_B:%.*]] = begin_borrow [[MAIN_ACTOR]]
// CHECK:   hop_to_executor [[MAIN_ACTOR_B]]
// CHECK:   apply [[FUNC]]([[ARG]])
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$s21attr_execution_silgen13SendableKlassCIeghg_ScA_pSgACIegHgILg_TRScMTU'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s21attr_execution_silgen13SendableKlassCIeghg_ACIegHg_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed SendableKlass) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guaranteed $@Sendable @callee_guaranteed (@guaranteed SendableKlass) -> ()):
// CHECK:   [[MAIN_ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK:   [[MAIN_ACTOR_B:%.*]] = begin_borrow [[MAIN_ACTOR]]
// CHECK:   hop_to_executor [[MAIN_ACTOR_B]]
// CHECK-NOT: hop_to_executor
// CHECK:   apply [[FUNC]]([[ARG]])
// CHECK: } // end sil function '$s21attr_execution_silgen13SendableKlassCIeghg_ACIegHg_TRScMTU'

func conversionsFromSyncToAsync(_ x: @escaping @Sendable (NonSendableKlass) -> Void,
                                _ y: @escaping @MainActor @Sendable (SendableKlass) -> Void) async {
  let _: nonisolated(nonsending) (NonSendableKlass) async -> Void = x
  let _: nonisolated(nonsending) (SendableKlass) async -> Void = y
  let _: @concurrent (SendableKlass) async -> Void = y
}

func testThatClosuresAssumeIsolation(fn: inout nonisolated(nonsending) (Int) async -> Void) {
  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFyyYaYCcfU_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
  // CHECK: bb0([[EXECUTOR:%.*]] : @guaranteed $Optional<any Actor>):
  // CHECK: hop_to_executor [[EXECUTOR]]
  let _: nonisolated(nonsending) () async -> Void = {
    42
  }

  func testParam(_: nonisolated(nonsending) () async throws -> Void) {}

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFyyYaYCXEfU0_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> @error any Error {
  // CHECK: bb0([[EXECUTOR:%.*]] : @guaranteed $Optional<any Actor>):
  // CHECK: hop_to_executor [[EXECUTOR]]
  // CHECK: } // end sil function '$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFyyYaYCXEfU0_'
  testParam { 42 }

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFyyYaXEfU1_ : $@convention(thin) @async () -> ()
  // CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
  // CHECK: hop_to_executor [[GENERIC_EXECUTOR]]
  testParam { @concurrent in 42 }

  // CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIgH_ScA_pSgs5Error_pIegHgILzo_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[FUNC:%.*]] : @guaranteed $@noescape @async @callee_guaranteed () -> ()):
  // CHECK:   apply [[FUNC]]()
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$sIgH_ScA_pSgs5Error_pIegHgILzo_TR'

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFySiYaYCcfU2_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, Int) -> () {
  // CHECK: bb0([[EXECUTOR:%.*]] : @guaranteed $Optional<any Actor>, %1 : $Int):
  // CHECK: hop_to_executor [[EXECUTOR]]
  fn = { _ in }

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFySiYacfU3_ : $@convention(thin) @async (Int) -> ()
  // CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
  // CHECK: hop_to_executor [[GENERIC_EXECUTOR]]
  // CHECK: } // end sil function '$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFySiYacfU3_'

  // CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSiIegHy_ScA_pSgSiIegHgILy_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, Int, @guaranteed @async @callee_guaranteed (Int) -> ()) -> () {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : $Int, [[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (Int) -> ()):
  // CHECK:  apply [[FUNC]]([[ARG]])
  // CHECK:  hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$sSiIegHy_ScA_pSgSiIegHgILy_TR'
  fn = { @concurrent _ in }
}

@MainActor
func testNoIsolationTransfer() {
  // CHECK: // Isolation: global_actor. type: MainActor
  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen23testNoIsolationTransferyyF0D7ErasureL_yyyyYaYAcF : $@convention(thin) (@guaranteed @isolated(any) @async @callee_guaranteed () -> ()) -> ()
  func testErasure(@_inheritActorContext _: @escaping @isolated(any) () async -> Void) {}

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen23testNoIsolationTransferyyFyyYacfU_ : $@convention(thin) @async (@guaranteed Optional<any Actor>) -> ()
  // CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
  // CHECK: hop_to_executor [[GENERIC_EXECUTOR]]
  testErasure { @concurrent in }
}

func testClosuresDontAssumeGlobalActorWithMarkedAsConcurrent() {
  func test(_ fn: @MainActor () async -> Void) {}

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen55testClosuresDontAssumeGlobalActorWithMarkedAsConcurrentyyFyyYaYbXEfU_
  // CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
  // CHECK-NEXT: hop_to_executor [[GENERIC_EXECUTOR]]
  // CHECK: } // end sil function '$s21attr_execution_silgen55testClosuresDontAssumeGlobalActorWithMarkedAsConcurrentyyFyyYaYbXEfU_'
  test { @Sendable @concurrent in
  }
}

nonisolated(nonsending)
public func takesCallerIsolatedThrowingFunction<T>(
  _ operation: nonisolated(nonsending) () async throws -> T
) async rethrows -> T {
  try await operation()
}

func observe() {}

// Test that we emit closures with nonisolated(nonsending) isolation without
// introducing an intermediate @concurrent closure function.
func testConvertToThrowing(isolation: isolated (any Actor)? = #isolation) async {
  // CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen21testConvertToThrowing9isolationyScA_pSgYi_tYaF :
  // CHECK:         [[ACTOR_COPY:%.*]] = copy_value %0
  // CHECK-NEXT:    [[ACTOR_BORROW:%.*]] = begin_borrow [[ACTOR_COPY]]
  // CHECK-NEXT:    hop_to_executor [[ACTOR_BORROW]]
  // CHECK:         [[CLOSURE:%.*]] = function_ref @$s21attr_execution_silgen21testConvertToThrowing9isolationyScA_pSgYi_tYaFyyYaYCXEfU_ :
  // CHECK-NEXT:    [[CLOSURE_VALUE:%.*]] = thin_to_thick_function [[CLOSURE]] to
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    [[FN:%.*]] = function_ref
  // CHECK-NEXT:    try_apply [[FN]]<()>({{%.*}}, {{%.*}}, [[CLOSURE_VALUE]]) {{.*}}, normal bb1, error bb2
  // CHECK:       bb1(
  //   This hop is unnecessary because nonisolated(nonsending) should
  //   preserve isolation on return.
  // CHECK-NEXT:    hop_to_executor [[ACTOR_BORROW]]

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen21testConvertToThrowing9isolationyScA_pSgYi_tYaFyyYaYCXEfU_ : $@convention(thin) @async @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> (@out τ_0_0, @error any Error) for <()>
  // CHECK:      bb0(
  // CHECK-NEXT:   debug_value
  //   This hop is unnecessary because nonisolated(nonsending) should
  //   ensure isolation before call.
  // CHECK-NEXT:   hop_to_executor %1
  // CHECK-NEXT:   // function_ref observe()
  // CHECK-NEXT:   [[FN:%.*]] = function_ref @$s21attr_execution_silgen7observeyyF :
  // CHECK-NEXT:   apply [[FN]]()

  await takesCallerIsolatedThrowingFunction {
    observe()
  }
}
