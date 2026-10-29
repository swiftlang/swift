// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls %s -module-name main | %FileCheck %s
// RUN: %if OS=macosx %{ %target-swift-frontend -emit-silgen -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls %s -module-name main | %FileCheck %s --check-prefix=SYNC %}
// RUN: %if OS=macosx %{ %target-swift-frontend -emit-sil -Onone -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls %s -module-name main | %FileCheck %s --check-prefix=HELPER %}
// RUN: %if OS=macosx %{ %target-swift-frontend -emit-ir -O -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls %s -module-name main | %FileCheck %s --check-prefix=IR %}

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// A 'send' of an 'async' function creates a task at the call site (CHECK). A
// 'send' of a synchronous function is enqueued through the '_enqueueOneway*'
// helpers instead (SYNC), which create a task whose operation is synchronous
// where the runtime supports it, and fall back to a task with an async
// operation before Swift 6.5 (HELPER, IR)

// ==== -----------------------------------------------------------------------
// MARK: 'send' of an 'async' function

actor Greeter {
  func greet() async {}
}

@MainActor
final class GlobalGreeter {
  func greet() async {}
}

// 'send' lowers to a discarding 'createAsyncTask': flags carry
// CopyTaskLocals|EnqueueJob|IsDiscardingTask|IsTaskFunctionConsumed
// (== 54272), no future fragment. The operation closure is called exactly
// once, by the task, and consumes its context, which holds the receiver and
// the arguments that were evaluated at the call site. On a plain
// actor-instance target, the actor's own serial executor is derived from
// the evaluated receiver via 'buildDefaultActorExecutorRef' and passed as
// the task's initial serial executor, so successive 'send' calls to the same
// actor enqueue in program order (FIFO)
// CHECK-LABEL: sil hidden [ossa] @$s4main4testyyAA7GreeterCKF
func test(_ g: Greeter) throws {
  // CHECK: [[RECEIVER:%.*]] = copy_value %0
  // CHECK: [[BASE:%.*]] = move_value [lexical] [var_decl] [[RECEIVER]]
  // CHECK: [[BASE_BORROW:%.*]] = begin_borrow [[BASE]]
  // CHECK: [[ACTOR:%.*]] = copy_value [[BASE_BORROW]]
  // CHECK: [[BORROW:%.*]] = begin_borrow [[ACTOR]]
  // CHECK: [[EXEC:%.*]] = builtin "buildDefaultActorExecutorRef"<Greeter>([[BORROW]] : $Greeter) : $Builtin.Executor
  // CHECK: [[SOME_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[EXEC]] : $Builtin.Executor
  // CHECK: [[OPERATION_FN:%.*]] = function_ref @$s4main4testyyAA7GreeterCKFyyYaKXOfU_ : $@convention(thin) @async (@sil_sending @guaranteed Greeter) -> @error any Error
  // CHECK: [[BASE_COPY:%.*]] = copy_value [[BASE]]
  // CHECK: [[OPERATION:%.*]] = partial_apply [called_once] [[OPERATION_FN]]([[BASE_COPY]])
  // CHECK: [[FLAGS_INT:%.*]] = integer_literal $Builtin.Int64, 54272
  // CHECK: [[FLAGS:%.*]] = struct $Int ([[FLAGS_INT]]
  // CHECK: builtin "createAsyncTask"([[FLAGS]] : $Int, [[SOME_EXEC]] : $Optional<Builtin.Executor>, {{.*}}, [[OPERATION]] :
  send g.greet()
}

// The operation closure receives the evaluated receiver as a 'sending'
// capture and makes the call
// CHECK-LABEL: sil private [ossa] @$s4main4testyyAA7GreeterCKFyyYaKXOfU_ : $@convention(thin) @async (@sil_sending @guaranteed Greeter) -> @error any Error {
// CHECK: bb0([[CAPTURED:%.*]] : @closureCapture @guaranteed $Greeter):
// CHECK: class_method [[CAPTURED]] : $Greeter, #Greeter.greet

// On a global-actor-isolated target, the global actor's own executor is
// derived (the main actor has a dedicated builtin, since it isn't a
// default-actor executor) and set as the initial serial executor too
// CHECK-LABEL: sil hidden [ossa] @$s4main10testGlobalyyAA0C7GreeterCKF
func testGlobal(_ g: GlobalGreeter) throws {
  // CHECK: [[EXEC:%.*]] = builtin "buildMainActorExecutorRef"() : $Builtin.Executor
  // CHECK: [[SOME_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.some!enumelt, [[EXEC]] : $Builtin.Executor
  // CHECK: builtin "createAsyncTask"({{.*}} : $Int, [[SOME_EXEC]] : $Optional<Builtin.Executor>,
  send g.greet()
}

// ==== -----------------------------------------------------------------------
// MARK: 'send' of a synchronous function

public actor A {
  public func tell(_ x: Int) {}
  public func tellAsync(_ x: Int) async {}
}

@MainActor public func onMain(_ x: Int) {}

// SYNC-LABEL: sil [ossa] @$s4main4test1ayAA1AC_tYaF :
// SYNC:         function_ref @$ss14_enqueueOneway2on_yx_yxYictScARzlF
// SYNC:         [[ASYNC_OP:%.*]] = function_ref @$s4main4test1ayAA1AC_tYaFyyYaKXOfU0_ : $@convention(thin) @async
// SYNC:         builtin "createAsyncTask"
// SYNC-NOT:     builtin "createAsyncTask"
// SYNC:         function_ref @$ss25_enqueueOnewayIsolatedAnyyyyyYAcF
// SYNC:       } // end sil function '$s4main4test1ayAA1AC_tYaF'
public func test(a: A) async {
  send a.tell(1)
  send a.tellAsync(2)
  send onMain(4)
}

// Below Swift 6.5 the helper checks the availability of tasks with a
// synchronous operation at runtime and falls back to an async operation
// HELPER-LABEL: sil shared [export_implementation] {{.*}}@$ss23_enqueueOnewayUnchecked2on__ySce_xyxctRlzClF :
// HELPER:         [[BODY_FN:%.*]] = function_ref @$ss23_enqueueOnewayUnchecked2on__ySce_xyxctRlzClFyycfU_ :
// HELPER:         [[BODY:%.*]] = partial_apply [callee_guaranteed] [[BODY_FN]]
// HELPER:         function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// HELPER:         cond_br
// HELPER:         [[ASYNC_OP_FN:%.*]] = function_ref @$ss23_enqueueOnewayOperation2on_ySceSg_yycntF029_unsafeInheritExecutor_onewayC0L_yyYaKF : $@convention(thin) @async
// HELPER:         [[ASYNC_OP:%.*]] = partial_apply [callee_guaranteed] [[ASYNC_OP_FN]]([[BODY]])
// HELPER:         builtin "createAsyncTask"({{.*}}, [[ASYNC_OP]])
// HELPER:         builtin "createAsyncTask"({{.*}}, [[BODY]])
// HELPER:       } // end sil function '$ss23_enqueueOnewayUnchecked2on__ySce_xyxctRlzClF'

// The task whose operation is synchronous has the 'HasSynchronousOperation'
// flag (1 << 17); both pass the '()' metadata like any discarding task
// IR-DAG:    call swiftcc %swift.async_task_and_context @swift_task_create(i64 152576, ptr {{.*}}, ptr {{.*}}@"$sytN"{{.*}}, ptr
// IR-DAG:    call swiftcc %swift.async_task_and_context @swift_task_create(i64 21504, ptr {{.*}}, ptr {{.*}}@"$sytN"
