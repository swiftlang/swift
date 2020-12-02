// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 -enable-experimental-concurrency -parse-stdlib -sil-verify-all | %FileCheck %s
// REQUIRES: concurrency

import Swift
import _Concurrency

func getInt() async -> Int { 0 }
func getString() async -> String { "" }
func getStringThrowingly() async throws -> String { "" }
func getIntAndString() async -> (Int, String) { (5, "hello") }

enum SomeError: Error {
  case boom
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A11AsyncLetIntSiyYF : $@convention(thin) @async () -> Int
func testAsyncLetInt() async -> Int {
  // CHECK: [[I:%.*]] = mark_uninitialized [var] %0
  // CHECK: [[CLOSURE:%.*]] = function_ref @$s4test0A11AsyncLetIntSiyYFSiyYcfu_ : $@convention(thin) @async () -> Int
  // CHECK: [[THICK_CLOSURE:%.*]] = thin_to_thick_function [[CLOSURE]] : $@convention(thin) @async () -> Int to $@async @callee_guaranteed () -> Int
  // CHECK: [[REABSTRACT_THUNK:%.*]] = function_ref @$sSiIegHd_Sis5Error_pIegHrzo_TR : $@convention(thin) @async (@guaranteed @async @callee_guaranteed () -> Int) -> (@out Int, @error Error)
  // CHECK: [[REABSTRACT_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT_THUNK]]([[THICK_CLOSURE]]) : $@convention(thin) @async (@guaranteed @async @callee_guaranteed () -> Int) -> (@out Int, @error Error)
  // CHECK: [[CLOSURE_ARG:%.*]] = convert_function [[REABSTRACT_CLOSURE]] : $@async @callee_guaranteed () -> (@out Int, @error Error) to $@async @callee_guaranteed @substituted <τ_0_0> () -> (@out τ_0_0, @error Error) for <Int>
  // CHECK: [[RUN_CHILD_TASK:%.*]] = function_ref @$s12_Concurrency13_runChildTask9operationBoxyYKc_tYlF : $@convention(thin) @async <τ_0_0> (@guaranteed @async @callee_guaranteed @substituted <τ_0_0> () -> (@out τ_0_0, @error Error) for <τ_0_0>) -> @owned Builtin.NativeObject
  // CHECK: [[CHILD_TASK:%.*]] = apply [[RUN_CHILD_TASK]]<Int>([[CLOSURE_ARG]]) : $@convention(thin) @async <τ_0_0> (@guaranteed @async @callee_guaranteed @substituted <τ_0_0> () -> (@out τ_0_0, @error Error) for <τ_0_0>) -> @owned Builtin.NativeObject
  async let i = await getInt()

  // CHECK: [[FUTURE_GET:%.*]] = function_ref @$s12_Concurrency14_taskFutureGetyxBoYlF : $@convention(thin) @async <τ_0_0> (@guaranteed Builtin.NativeObject) -> @out τ_0_0
  // CHECK: [[INT_RESULT:%.*]] = alloc_stack $Int
  // CHECK: apply [[FUTURE_GET]]<Int>([[INT_RESULT]], [[CHILD_TASK]]) : $@convention(thin) @async <τ_0_0> (@guaranteed Builtin.NativeObject) -> @out τ_0_0
  // CHECK: [[INT_RESULT_VALUE:%.*]] = load [trivial] [[INT_RESULT]] : $*Int
  // CHECK: assign [[INT_RESULT_VALUE]] to [[I]] : $*Int
  return await i

  // CHECK: [[BORROW_CHILD_TASK:%.*]] = begin_borrow [[CHILD_TASK]] : $Builtin.NativeObject
  // CHECK-NEXT: builtin "cancelAsyncTask"([[BORROW_CHILD_TASK]] : $Builtin.NativeObject) : $()
  // CHECK-NEXT: end_borrow [[BORROW_CHILD_TASK]] : $Builtin.NativeObject
  
  // CHECK: destroy_value [[CHILD_TASK]] : $Builtin.NativeObject 
}

func testAsyncLetWithThrows(cond: Bool) async throws -> String {
  async let i = await getInt()
  async let s = await getString()

  if cond {
    throw SomeError.boom
  }
  
  return await s
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A14AsyncLetThrowsSSyYKF : $@convention(thin) @async () -> (@owned String, @error Error) {
func testAsyncLetThrows() async throws -> String {
  async let s = await try getStringThrowingly()

  // CHECK: [[RUN_CHILD_TASK:%.*]] = function_ref @$s12_Concurrency22_taskFutureGetThrowingyxBoYKlF : $@convention(thin) @async <τ_0_0> (@guaranteed Builtin.NativeObject) -> (@out τ_0_0, @error Error)
  // CHECK: try_apply [[RUN_CHILD_TASK]]<String>
  return await try s
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A14DecomposeAwait4condSiSb_tYF : $@convention(thin) @async (Bool) -> Int {
func testDecomposeAwait(cond: Bool) async -> Int {
  // CHECK: [[I_VAR:%.*]] = alloc_stack $Int, let, name "i"
  // CHECK: [[I:%.*]] = mark_uninitialized [var] [[I_VAR]] : $*Int
  // CHECK: [[S_VAR:%.*]] = alloc_stack $String, let, name "s"
  // CHECK: [[S:%.*]] = mark_uninitialized [var] [[S_VAR]] : $*String
  async let (i, s) = await getIntAndString()

  if cond {
    // CHECK: [[FUTURE_GET:%.*]] = function_ref @$s12_Concurrency14_taskFutureGetyxBoYlF : $@convention(thin) @async <τ_0_0> (@guaranteed Builtin.NativeObject) -> @out τ_0_0
    // CHECK: [[TUPLE_RESULT:%.*]] = alloc_stack $(Int, String)
    // CHECK: apply [[FUTURE_GET]]<(Int, String)>([[TUPLE_RESULT]], {{%.*}}) : $@convention(thin) @async <τ_0_0> (@guaranteed Builtin.NativeObject) -> @out τ_0_0
    // CHECK: [[TUPLE_RESULT_VAL:%.*]] = load [take] [[TUPLE_RESULT]] : $*(Int, String)
    // CHECK: ([[FIRST_VAL:%.*]], [[SECOND_VAL:%.*]]) = destructure_tuple [[TUPLE_RESULT_VAL]] : $(Int, String)
    // CHECK: assign [[FIRST_VAL]] to [[I]] : $*Int
    // CHECK: assign [[SECOND_VAL]] to [[S]] : $*String
    return await Int(s)!
  }

  return await i
}
