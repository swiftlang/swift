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

@MainActor
func globalGlobalActorIsolatedFunc() async -> () {}

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

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen33testCallerToConcurrentNonIsolatedyyyyYaYCcYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()):
// CHECK:   [[FUNC_COPY:%.*]] = copy_value [[FUNC]]
// CHECK:   [[THUNK:%.*]] = function_ref @$sBAIegHgIL_IegH_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
// CHECK:   partial_apply [callee_guaranteed] [[THUNK]]([[FUNC_COPY]])
// CHECK: } // end sil function '$s21attr_execution_silgen33testCallerToConcurrentNonIsolatedyyyyYaYCcYaF'
public func testCallerToConcurrentNonIsolated(_ x: nonisolated(nonsending) @escaping () async -> ()) async {
  let y: @concurrent () async -> () = x
  await y()
}

// This thunk is used to convert a caller to a concurrent function. So, we pass in .none as the
// isolated parameter.
//
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sBAIegHgIL_IegH_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()):
// CHECK:   [[ENUM:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK:   hop_to_executor [[ENUM]]
// CHECK:   [[CAST:%.*]] = unchecked_value_cast [[ENUM]] to $Builtin.ImplicitActor
// CHECK:   apply [[FUNC]]([[CAST]])
// CHECK: } // end sil function '$sBAIegHgIL_IegH_TR'

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen31testCallerToConcurrentMainActoryyyyYaYCcYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()):
// CHECK:   [[FUNC_COPY:%.*]] = copy_value [[FUNC]]
// CHECK:   [[THUNK:%.*]] = function_ref @$sBAIegHgIL_IegH_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
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
// CHECK:   [[THUNK:%.*]] = function_ref @$sIegH_BAIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   partial_apply [callee_guaranteed] [[THUNK]]([[FUNC_COPY]])
// CHECK: } // end sil function '$s21attr_execution_silgen33testConcurrentToCallerNonIsolatedyyyyYacYaF'
public func testConcurrentToCallerNonIsolated(_ x: @escaping @concurrent () async -> ()) async {
  let y: nonisolated(nonsending) () async -> () = x
  await y()
}

// This thunk has the "surface" of an @caller function, but just ignores the
// isolated parameter and calls the concurrent funciton.
//
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIegH_BAIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @async @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Builtin.ImplicitActor, [[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed () -> ()):
// CHECK:   apply [[FUNC]]
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$sIegH_BAIegHgIL_TR'

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen42testConcurrentToCallerNonIsolatedMainActoryyyyYacYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] :
// CHECK:   [[FUNC_COPY:%.*]] = copy_value [[FUNC]]
// CHECK:   [[THUNK:%.*]] = function_ref @$sIegH_BAIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @async @callee_guaranteed () -> ()) -> ()
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

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen012testCallerToE0yyyyYaYCcYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> () {
// CHECK: bb0([[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()):
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

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen24testCallerLocalVariablesyyyyYaYCcYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> () {
// CHECK: bb0([[PARAM:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()):
// CHECK:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   [[PARAM_COPY:%.*]] = copy_value [[PARAM]]
// CHECK:   [[Y:%.*]] = move_value [lexical] [var_decl] [[PARAM_COPY]]
// CHECK:   [[Y_B:%.*]] = begin_borrow [[Y]]
// CHECK:   [[Y_B_C:%.*]] = copy_value [[Y_B]]
// CHECK:   [[Y2:%.*]] = move_value [lexical] [var_decl] [[Y_B_C]]
// CHECK:   [[Y2_B:%.*]] = begin_borrow [[Y2]]
// CHECK:   [[Y2_B_C:%.*]] = copy_value [[Y2_B]]
// CHECK:   [[ACTOR_CAST:%.*]] = unchecked_value_cast [[ACTOR]] to $Builtin.ImplicitActor
// CHECK:   [[Y2_B_C_B:%.*]] = begin_borrow [[Y2_B_C]]
// CHECK:   apply [[Y2_B_C_B]]([[ACTOR_CAST]])
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

// CHECK-LABEL: sil [ossa] @$s21attr_execution_silgen34testCallerConcurrentLocalVariablesyyyyYaYCcYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> () {
// CHECK: bb0([[PARAM:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()):
// CHECK:   [[PARAM_COPY:%.*]] = copy_value [[PARAM]]
// CHECK:   [[THUNK_1:%.*]] = function_ref @$sBAIegHgIL_IegH_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK_1]]([[PARAM_COPY]])
// CHECK:   [[Y:%.*]] = move_value [lexical] [var_decl] [[PA]]
// CHECK:   [[Y_B:%.*]] = begin_borrow [[Y]]
// CHECK:   [[Y_B_C:%.*]] = copy_value [[Y_B]]
// CHECK:   [[THUNK_2:%.*]] = function_ref @$sIegH_BAIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @async @callee_guaranteed () -> ()) -> ()
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
// CHECK:   [[THUNK_1:%.*]] = function_ref @$sIegH_BAIegHgIL_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK_1]]([[PARAM_C]])
// CHECK:   [[Y:%.*]] = move_value [lexical] [var_decl] [[PA]]
// CHECK:   [[Y_B:%.*]] = begin_borrow [[Y]]
// CHECK:   [[Y_B_C:%.*]] = copy_value [[Y_B]]
// CHECK:   [[THUNK_2:%.*]] = function_ref @$sBAIegHgIL_IegH_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
// CHECK:   [[PA_2:%.*]] = partial_apply [callee_guaranteed] [[THUNK_2]]([[Y_B_C]])
// CHECK: } // end sil function '$s21attr_execution_silgen34testConcurrentCallerLocalVariablesyyyyYacYaF'
public func testConcurrentCallerLocalVariables(_ x: @escaping @concurrent () async -> ()) async {
  let y: nonisolated(nonsending) () async -> () = x
  let y2: @concurrent () async -> () = y
  await y2()
}

// CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen22globalActorConversionsyyyyYac_yyYaYCctYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed () -> (), @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> () {
// CHECK: bb0([[X:%.*]] : @guaranteed $@async @callee_guaranteed () -> (), [[Y:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()):
// CHECK:   [[GLOBAL_CALLER_FUNC:%.*]] = function_ref @$s21attr_execution_silgen16globalCallerFuncyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// FIVE:    [[TTFI:%.*]] = thin_to_thick_function [[GLOBAL_CALLER_FUNC]] to $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// FIVE:    [[THUNK:%.*]] = function_ref @$sBAIegHgIL_IegH_TRScMTU : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
// FIVE:    [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[TTFI]]) :
// SIX:     [[THUNK:%.*]] = function_ref @$sBAIetHgIL_IeghH_TRScMTU : $@convention(thin) @Sendable @async (@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
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
// FIVE:   [[THUNK:%.*]] = function_ref @$sBAIegHgIL_IegH_TRScMTU : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> ()
// FIVE:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])
// FIVE:   [[V4:%.*]] = move_value [lexical] [var_decl] [[PA]]
// FIVE:   [[V4_B:%.*]] = begin_borrow [[V4]]
// FIVE:   [[V4_B_C:%.*]] = copy_value [[V4_B]]
// FIVE:   [[V4_B_C_B:%.*]] = begin_borrow [[V4_B_C]]
// FIVE:   apply [[V4_B_C_B]]()

// CHECK: } // end sil function '$s21attr_execution_silgen22globalActorConversionsyyyyYac_yyYaYCctYaF'

// SIX-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sBAIetHgIL_IeghH_TRScMTU : $@convention(thin) @Sendable @async (@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> () {
// SIX: bb0([[FUNC:%.*]] : $@convention(thin) @async (@sil_isolated
// SIX:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// SIX:   [[E:%.*]] = init_existential_ref [[ACTOR]] : $MainActor : $MainActor, $any Actor
// SIX:   [[E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[E]]
// SIX:   hop_to_executor [[E_OPT]]
// SIX:   [[E_OPT_B:%.*]] = begin_borrow [[E_OPT]]
// SIX:   [[E_OPT_B_CAST:%.*]] = unchecked_value_cast [[E_OPT_B]] to $Builtin.ImplicitActor
// SIX:   apply [[FUNC]]([[E_OPT_B_CAST]]) : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// SIX: } // end sil function '$sBAIetHgIL_IeghH_TRScMTU'

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

// CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen23globalActorConversions2yyyAA13SendableKlassCYac_yADYaYCctYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@guaranteed SendableKlass) -> (), @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()) -> () {
// CHECK: bb0([[X:%.*]] : @guaranteed $@async @callee_guaranteed (@guaranteed SendableKlass) -> (), [[Y:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()):
// CHECK:   [[GLOBAL_CALLER_FUNC:%.*]] = function_ref @$s21attr_execution_silgen29globalCallerFuncSendableKlassyyAA0gH0CYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()
// FIVE:    [[TTFI:%.*]] = thin_to_thick_function [[GLOBAL_CALLER_FUNC]] to $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()
// FIVE:    [[THUNK:%.*]] = function_ref @$sBA21attr_execution_silgen13SendableKlassCIegHgILg_ACIegHg_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()) -> ()
// FIVE:    [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[TTFI]]) :
// SIX:     [[THUNK:%.*]] = function_ref @$sBA21attr_execution_silgen13SendableKlassCIetHgILg_ACIeghHg_TRScMTU : $@convention(thin) @Sendable @async (@guaranteed SendableKlass, @convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()) -> ()
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
// FIVE:   [[THUNK:%.*]] = function_ref @$sBA21attr_execution_silgen13SendableKlassCIegHgILg_ACIegHg_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()) -> ()
// FIVE:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])
// FIVE:   [[V4:%.*]] = move_value [lexical] [var_decl] [[PA]]
// FIVE:   [[V4_B:%.*]] = begin_borrow [[V4]]
// FIVE:   [[V4_B_C:%.*]] = copy_value [[V4_B]]
// FIVE:   [[V4_B_C_B:%.*]] = begin_borrow [[V4_B_C]]
// FIVE:   apply [[V4_B_C_B]]({{%.*}})

// CHECK: } // end sil function '$s21attr_execution_silgen23globalActorConversions2yyyAA13SendableKlassCYac_yADYaYCctYaF'

// FIVE-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sBA21attr_execution_silgen13SendableKlassCIegHgILg_ACIegHg_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()) -> () {
// FIVE: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guaranteed
// FIVE:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// FIVE:   [[E:%.*]] = init_existential_ref [[ACTOR]] : $MainActor : $MainActor, $any Actor
// FIVE:   [[E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[E]]
// FIVE:   hop_to_executor [[E_OPT]]
// FIVE:   [[E_OPT_B:%.*]] = begin_borrow [[E_OPT]]
// FIVE:   [[E_OPT_B_CAST:%.*]] = unchecked_value_cast [[E_OPT_B]] to $Builtin.ImplicitActor
// FIVE:   apply [[FUNC]]([[E_OPT_B_CAST]], [[ARG]])
// FIVE: } // end sil function '$sBA21attr_execution_silgen13SendableKlassCIegHgILg_ACIegHg_TRScMTU'

// SIX-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sBA21attr_execution_silgen13SendableKlassCIetHgILg_ACIeghHg_TRScMTU : $@convention(thin) @Sendable @async (@guaranteed SendableKlass, @convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()) -> () {
// SIX: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()):
// SIX:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// SIX:   [[E:%.*]] = init_existential_ref [[ACTOR]] : $MainActor : $MainActor, $any Actor
// SIX:   [[E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[E]]
// SIX:   hop_to_executor [[E_OPT]]
// SIX:   [[E_OPT_B:%.*]] = begin_borrow [[E_OPT]]
// SIX:   [[E_OPT_B_CAST:%.*]] = unchecked_value_cast [[E_OPT_B]] to $Builtin.ImplicitActor
// SIX:   apply [[FUNC]]([[E_OPT_B_CAST]], [[ARG]])
// SIX: } // end sil function '$sBA21attr_execution_silgen13SendableKlassCIetHgILg_ACIeghHg_TRScMTU'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sBA21attr_execution_silgen13SendableKlassCIegHgILg_ACIegHg_TR : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guaranteed
// CHECK:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   [[ACTOR_CAST:%.*]] = unchecked_value_cast [[ACTOR]] to $Builtin.ImplicitActor
// CHECK:   apply [[FUNC]]([[ACTOR_CAST]], [[ARG]])
// CHECK: } // end sil function '$sBA21attr_execution_silgen13SendableKlassCIegHgILg_ACIegHg_TR'
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

// CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen23globalActorConversions3yyAA13SendableKlassCADYac_A2DYaYCctYaF : $@convention(thin) @async (@guaranteed @async @callee_guaranteed (@guaranteed SendableKlass) -> @owned SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass) -> () {
// CHECK: bb0([[X:%.*]] : @guaranteed $@async @callee_guaranteed (@guaranteed SendableKlass) -> @owned SendableKlass, [[Y:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass):
// CHECK:   [[GLOBAL_CALLER_FUNC:%.*]] = function_ref @$s21attr_execution_silgen29globalCallerFuncSendableKlassyAA0gH0CADYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass
// FIVE:    [[TTFI:%.*]] = thin_to_thick_function [[GLOBAL_CALLER_FUNC]] to $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass
// FIVE:    [[THUNK:%.*]] = function_ref @$sBA21attr_execution_silgen13SendableKlassCACIegHgILgo_A2CIegHgo_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass
// FIVE:    [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[TTFI]])
// SIX:     [[THUNK:%.*]] = function_ref @$sBA21attr_execution_silgen13SendableKlassCACIetHgILgo_A2CIeghHgo_TRScMTU : $@convention(thin) @Sendable @async (@guaranteed SendableKlass, @convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass
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
// FIVE:   [[THUNK:%.*]] = function_ref @$sBA21attr_execution_silgen13SendableKlassCACIegHgILgo_A2CIegHgo_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass
// FIVE:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])
// FIVE:   [[V4:%.*]] = move_value [lexical] [var_decl] [[PA]]
// FIVE:   [[V4_B:%.*]] = begin_borrow [[V4]]
// FIVE:   [[V4_B_C:%.*]] = copy_value [[V4_B]]
// FIVE:   [[V4_B_C_B:%.*]] = begin_borrow [[V4_B_C]]
// FIVE:   apply [[V4_B_C_B]]({{%.*}})

// CHECK:  [[Y_C:%.*]] = copy_value [[Y]]
// CHECK:  [[THUNK:%.*]] = function_ref @$sBA21attr_execution_silgen13SendableKlassCACIegHgILgo_A2CIegHgo_TR : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass
// CHECK:  [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])
// CHECK:  [[V5:%.*]] = move_value [lexical] [var_decl] [[PA]]
// CHECK: } // end sil function '$s21attr_execution_silgen23globalActorConversions3yyAA13SendableKlassCADYac_A2DYaYCctYaF'

// FIVE-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sBA21attr_execution_silgen13SendableKlassCACIegHgILgo_A2CIegHgo_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass {
// FIVE: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guaranteed
// FIVE:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// FIVE:   [[E:%.*]] = init_existential_ref [[ACTOR]] : $MainActor : $MainActor, $any Actor
// FIVE:   [[E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[E]]
// FIVE:   hop_to_executor [[E_OPT]]
// FIVE:   [[E_OPT_B:%.*]] = begin_borrow [[E_OPT]]
// FIVE:  [[E_OPT_B_CAST:%.*]] = unchecked_value_cast [[E_OPT_B]] to $Builtin.ImplicitActor
// FIVE:   apply [[FUNC]]([[E_OPT_B_CAST]], [[ARG]])
// FIVE: } // end sil function '$sBA21attr_execution_silgen13SendableKlassCACIegHgILgo_A2CIegHgo_TRScMTU'

// SIX-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sBA21attr_execution_silgen13SendableKlassCACIetHgILgo_A2CIeghHgo_TRScMTU : $@convention(thin) @Sendable @async (@guaranteed SendableKlass, @convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass {
// SIX: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass):
// SIX:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// SIX:   [[E:%.*]] = init_existential_ref [[ACTOR]] : $MainActor : $MainActor, $any Actor
// SIX:   [[E_OPT:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[E]]
// SIX:   hop_to_executor [[E_OPT]]
// SIX:   [[E_OPT_B:%.*]] = begin_borrow [[E_OPT]]
// SIX:   [[E_OPT_B_CAST:%.*]] = unchecked_value_cast [[E_OPT_B]] to $Builtin.ImplicitActor
// SIX:   apply [[FUNC]]([[E_OPT_B_CAST]], [[ARG]])
// SIX: } // end sil function '$sBA21attr_execution_silgen13SendableKlassCACIetHgILgo_A2CIeghHgo_TRScMTU'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sBA21attr_execution_silgen13SendableKlassCACIegHgILgo_A2CIegHgo_TR : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass) -> @owned SendableKlass) -> @owned SendableKlass {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guarantee
// CHECK: [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK: hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$sBA21attr_execution_silgen13SendableKlassCACIegHgILgo_A2CIegHgo_TR'
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
// CHECK:   [[THUNK:%.*]] = function_ref @$s21attr_execution_silgen16NonSendableKlassCIeghg_BAACIegHgILg_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed NonSendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed NonSendableKlass) -> ()) -> ()
// CHECK:   [[PAI:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[X_C]])

// CHECK:   [[Y_C:%.*]] = copy_value [[Y]]
// CHECK:   [[THUNK:%.*]] = function_ref @$s21attr_execution_silgen13SendableKlassCIeghg_BAACIegHgILg_TRScMTU : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed SendableKlass) -> ()) -> ()
// CHECK:   [[PAI:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])

// CHECK:   [[Y_C:%.*]] = copy_value [[Y]]
// CHECK:   [[THUNK:%.*]] = function_ref @$s21attr_execution_silgen13SendableKlassCIeghg_ACIegHg_TRScMTU : $@convention(thin) @async (@guaranteed SendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed SendableKlass) -> ()) -> ()
// CHECK:   [[PAI:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[Y_C]])

// CHECK: } // end sil function '$s21attr_execution_silgen26conversionsFromSyncToAsyncyyyAA16NonSendableKlassCYbc_yAA0jK0CYbScMYcctYaF'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s21attr_execution_silgen16NonSendableKlassCIeghg_BAACIegHgILg_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed NonSendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed NonSendableKlass) -> ()) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Builtin.ImplicitActor
// CHECK: apply {{%.*}}({{%.*}}) : $@Sendable @callee_guaranteed (@guaranteed NonSendableKlass) -> ()
// CHECK: hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$s21attr_execution_silgen16NonSendableKlassCIeghg_BAACIegHgILg_TR'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s21attr_execution_silgen13SendableKlassCIeghg_BAACIegHgILg_TRScMTU : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed SendableKlass, @guaranteed @Sendable @callee_guaranteed (@guaranteed SendableKlass) -> ()) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Builtin.ImplicitActor, [[ARG:%.*]] : @guaranteed $SendableKlass, [[FUNC:%.*]] : @guaranteed $@Sendable @callee_guaranteed
// CHECK:   [[MAIN_ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK:   [[MAIN_ACTOR_B:%.*]] = begin_borrow [[MAIN_ACTOR]]
// CHECK:   hop_to_executor [[MAIN_ACTOR_B]]
// CHECK:   apply [[FUNC]]([[ARG]])
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK: } // end sil function '$s21attr_execution_silgen13SendableKlassCIeghg_BAACIegHgILg_TRScMTU'

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
  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFyyYaYCcfU_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> () {
  // CHECK: bb0([[EXECUTOR:%.*]] : @guaranteed $Builtin.ImplicitActor):
  // CHECK: hop_to_executor [[EXECUTOR]]
  let _: nonisolated(nonsending) () async -> Void = {
    _ = 42
  }

  func testParam(_: nonisolated(nonsending) () async throws -> Void) {}

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFyyYaYCXEfU0_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> @error any Error {
  // CHECK: bb0([[EXECUTOR:%.*]] : @guaranteed $Builtin.ImplicitActor):
  // CHECK: hop_to_executor [[EXECUTOR]]
  // CHECK: } // end sil function '$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFyyYaYCXEfU0_'
  testParam { _ = 42 }

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFyyYaXEfU1_ : $@convention(thin) @async () -> ()
  // CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
  // CHECK: hop_to_executor [[GENERIC_EXECUTOR]]
  // CHECK: } // end sil function '$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFyyYaXEfU1_'
  testParam { @concurrent in _ = 42 }

  // CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIgH_BAs5Error_pIegHgILzo_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> @error any Error {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Builtin.ImplicitActor, [[FUNC:%.*]] : @guaranteed $@noescape @async @callee_guaranteed () -> ()):
  // CHECK:   apply [[FUNC]]()
  // CHECK:   hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$sIgH_BAs5Error_pIegHgILzo_TR'

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFySiYaYCcfU2_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, Int) -> () {
  // CHECK: bb0([[EXECUTOR:%.*]] : @guaranteed $Builtin.ImplicitActor, %1 : $Int):
  // CHECK: hop_to_executor [[EXECUTOR]]
  fn = { _ in }

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFySiYacfU3_ : $@convention(thin) @async (Int) -> ()
  // CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
  // CHECK: hop_to_executor [[GENERIC_EXECUTOR]]
  // CHECK: } // end sil function '$s21attr_execution_silgen31testThatClosuresAssumeIsolation2fnyySiYaYCcz_tFySiYacfU3_'

  // CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSiIegHy_BASiIegHgILy_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, Int, @guaranteed @async @callee_guaranteed (Int) -> ()) -> () {
  // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Builtin.ImplicitActor, [[ARG:%.*]] : $Int, [[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (Int) -> ()):
  // CHECK:  apply [[FUNC]]([[ARG]])
  // CHECK:  hop_to_executor [[ACTOR]]
  // CHECK: } // end sil function '$sSiIegHy_BASiIegHgILy_TR'
  fn = { @concurrent _ in }
}

@MainActor
func testNoIsolationTransfer() {
  // CHECK: // Isolation: global_actor. type: MainActor
  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen23testNoIsolationTransferyyF0D7ErasureL_yyyyYaYAcF : $@convention(thin) (@guaranteed @isolated(any) @async @callee_guaranteed () -> ()) -> ()
  func testErasure(@_inheritActorContext _: @escaping @isolated(any) () async -> Void) {}

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen23testNoIsolationTransferyyFyyYacfU_ : $@convention(thin) @async (@guaranteed Optional<any Actor>) -> () {
  // CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
  // CHECK: hop_to_executor [[GENERIC_EXECUTOR]]
  testErasure { @concurrent in }
}

func testClosuresDontAssumeGlobalActorWithMarkedAsConcurrent() {
  func test(_ fn: @MainActor () async -> Void) {}

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen55testClosuresDontAssumeGlobalActorWithMarkedAsConcurrentyyFyyYaYbXEfU_
  // CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
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
  // CHECK-NEXT:    [[ACTOR_BORROW_CAST:%.*]] = unchecked_value_cast [[ACTOR_BORROW]]
  // CHECK-NEXT:    // function_ref
  // CHECK-NEXT:    [[FN:%.*]] = function_ref
  // CHECK-NEXT:    try_apply [[FN]]<()>({{%.*}}, [[ACTOR_BORROW_CAST]], [[CLOSURE_VALUE]]) {{.*}}, normal bb1, error bb2
  // CHECK:       bb1(
  //   This hop is unnecessary because nonisolated(nonsending) should
  //   preserve isolation on return.
  // CHECK-NEXT:    hop_to_executor [[ACTOR_BORROW]]

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen21testConvertToThrowing9isolationyScA_pSgYi_tYaFyyYaYCXEfU_ : $@convention(thin) @async @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> (@out τ_0_0, @error any Error) for <()>
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

func testSendableClosureNonisolatedNonSendingInference() {
  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen49testSendableClosureNonisolatedNonSendingInferenceyyFySiYaYbYCcfU_ : $@convention(thin) @Sendable @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, Int) -> ()
  // CHECK: bb0([[EXECUTOR:%.*]] : @guaranteed $Builtin.ImplicitActor, %1 : $Int):
  // CHECK: hop_to_executor [[EXECUTOR]]
  // CHECK: // end sil function '$s21attr_execution_silgen49testSendableClosureNonisolatedNonSendingInferenceyyFySiYaYbYCcfU_'
  let _: nonisolated(nonsending) @Sendable (Int) async -> Void = { _ in }

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen49testSendableClosureNonisolatedNonSendingInferenceyyFyS2SYaKYCcYaYbYCcfU0_ : $@convention(thin) @Sendable @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed String) -> (@owned String, @error any Error)) -> @error any Error
  // CHECK: bb0([[EXECUTOR:%.*]] : @guaranteed $Builtin.ImplicitActor, %1 : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed String) -> (@owned String, @error any Error)):
  // CHECK: hop_to_executor [[EXECUTOR]]
  // CHECK: // end sil function '$s21attr_execution_silgen49testSendableClosureNonisolatedNonSendingInferenceyyFyS2SYaKYCcYaYbYCcfU0_'
  let _: nonisolated(nonsending) @Sendable (
    nonisolated(nonsending) @escaping (String) async throws -> String
  ) async throws -> Void = { _ in }
}

// CHECK-LABEL: sil hidden [ossa] @$s21attr_execution_silgen014testSendableToE35ConversionWithNonisilatedNonsendingyyF : $@convention(thin) () -> ()
// CHECK: [[TEST_REF:%.*]] = function_ref @$s21attr_execution_silgen014testSendableToE35ConversionWithNonisilatedNonsendingyyF0D0L_7closureyS2SYaKYCc_tYaYbKF : $@convention(thin) @Sendable @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed String) -> (@owned String, @error any Error)) -> @error any Error
// CHECK: // end sil function '$s21attr_execution_silgen014testSendableToE35ConversionWithNonisilatedNonsendingyyF'
func testSendableToSendableConversionWithNonisilatedNonsending() {
  @Sendable nonisolated(nonsending) func test(
    closure: nonisolated(nonsending) @escaping (String) async throws -> String
  ) async throws {
  }

  let _: nonisolated(nonsending) @Sendable (
    nonisolated(nonsending) @escaping (String) async throws -> String
  ) async throws -> Void = test
}

func testNonisolatedNonsendingClosureInGlobalActorContext() {
  class NonSendable {
    var state = ""
  }

  struct S {
    static func compute(closure: nonisolated(nonsending) @Sendable @escaping (sending NonSendable) async -> Void) async {}
  }

  // CHECK-LABEL: sil private [ossa] @$s21attr_execution_silgen52testNonisolatedNonsendingClosureInGlobalActorContextyyF0D0L_yyYaF : $@convention(thin) @async () -> ()
  // CHECK: [[CLOSURE:%.*]] = function_ref @$s21attr_execution_silgen52testNonisolatedNonsendingClosureInGlobalActorContextyyF0D0L_yyYaFyAaByyF11NonSendableL_CYuYaYbYCcfU_ : $@convention(thin) @Sendable @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @sil_sending @guaranteed NonSendable) -> ()
  // CHECK: [[THICK_CLOSURE:%.*]] = thin_to_thick_function [[CLOSURE]] to $@Sendable @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @sil_sending @guaranteed NonSendable) -> ()
  // CHECK: [[CLOSURE_THUNK:%.*]] = function_ref @$sBA21attr_execution_silgen52testNonisolatedNonsendingClosureInGlobalActorContextyyF11NonSendableL_CIeghHgILgT_BAADIeghHgILxT_TR : $@convention(thin) @Sendable @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @sil_sending @owned NonSendable, @guaranteed @Sendable @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @sil_sending @guaranteed NonSendable) -> ()) -> ()
  // CHECK: [[THUNKED_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_THUNK]]([[THICK_CLOSURE]])
  // CHECK: [[COMPUTE:%.*]] = function_ref @$s21attr_execution_silgen52testNonisolatedNonsendingClosureInGlobalActorContextyyF1SL_V7compute7closureyyAaByyF11NonSendableL_CnYuYaYbYCc_tYaFZ : $@convention(method) @async (@guaranteed @Sendable @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @sil_sending @owned NonSendable) -> (), @thin S.Type) -> ()
  // CHECK: apply [[COMPUTE]]([[THUNKED_CLOSURE]], {{.*}}) : $@convention(method) @async (@guaranteed @Sendable @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @sil_sending @owned NonSendable) -> (), @thin S.Type) -> ()
  // CHECK: } // end sil function '$s21attr_execution_silgen52testNonisolatedNonsendingClosureInGlobalActorContextyyF0D0L_yyYaF'
  @MainActor
  func test() async {
    // CHECK: // closure #1 in test #1 () in testNonisolatedNonsendingClosureInGlobalActorContext()
    // CHECK: // Isolation: caller_isolation_inheriting
    await S.compute { _ in
    }
  }
}

// CHECK-LABEL: // localVariableAssignmentConversion()
// CHECK: // Isolation: caller_isolation_inheriting
// CHECK: sil hidden [ossa] @$s21attr_execution_silgen33localVariableAssignmentConversionyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed
// CHECK:   [[GLOBAL_ACTOR_FUNC:%.*]] = function_ref @$s21attr_execution_silgen29globalGlobalActorIsolatedFuncyyYaF : $@convention(thin) @async () -> ()
// FIVE:    [[GLOBAL_ACTOR_FUNC_TT_CVT:%.*]] = thin_to_thick_function [[GLOBAL_ACTOR_FUNC]] to $@async @callee_guaranteed () -> ()
// SIX:     [[GLOBAL_ACTOR_FUNC_TT:%.*]] = thin_to_thick_function [[GLOBAL_ACTOR_FUNC]] to $@async @callee_guaranteed () -> ()
// SIX:     [[GLOBAL_ACTOR_FUNC_TT_CVT:%.*]] = convert_function [[GLOBAL_ACTOR_FUNC_TT]] to $@Sendable @async @callee_guaranteed () -> ()
// CHECK:   [[MV_VALUE:%.*]] = move_value [lexical] [var_decl] [[GLOBAL_ACTOR_FUNC_TT_CVT]]
// CHECK:   debug_value [[MV_VALUE]], let, name "c1"

// CHECK:   [[NONISOLATED_NONSENDING_FUNC:%.*]] = function_ref @$s21attr_execution_silgen16globalCallerFuncyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// FIVE:    [[NONISOLATED_NONSENDING_FUNC_CVT_TT:%.*]] = thin_to_thick_function [[NONISOLATED_NONSENDING_FUNC]] to $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// SIX:     [[NONISOLATED_NONSENDING_FUNC_CVT:%.*]] = convert_function [[NONISOLATED_NONSENDING_FUNC]] to $@convention(thin) @Sendable @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// SIX:     [[NONISOLATED_NONSENDING_FUNC_CVT_TT:%.*]] = thin_to_thick_function [[NONISOLATED_NONSENDING_FUNC_CVT]] to $@Sendable @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// CHECK:   [[MV_VALUE:%.*]] = move_value [lexical] [var_decl] [[NONISOLATED_NONSENDING_FUNC_CVT_TT]]
// CHECK:   debug_value [[MV_VALUE]], let, name "c2"
// CHECK: } // end sil function '$s21attr_execution_silgen33localVariableAssignmentConversionyyYaF'
nonisolated(nonsending) func localVariableAssignmentConversion() async {
  let c1 = globalGlobalActorIsolatedFunc
  await c1()
  let c2 = globalCallerFunc
  await c2()
}
