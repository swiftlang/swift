// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -emit-silgen -I %S/Inputs/custom-modules  -target %target-swift-5.1-abi-triple %s -verify | %FileCheck --implicit-check-not=hop_to_executor --check-prefix=CHECK --check-prefix=CHECK-%target-cpu --check-prefix CHECK-C %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-upcoming-feature NonisolatedNonsendingByDefault -Xllvm -sil-print-types -emit-silgen -I %S/Inputs/custom-modules  -target %target-swift-5.1-abi-triple %s -verify | %FileCheck --implicit-check-not=hop_to_executor --check-prefix=CHECK --check-prefix=CHECK-%target-cpu --check-prefix CHECK-NN %s

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

import Foundation
import ObjCConcurrency

@objc protocol SlowServing {
    func requestInt() async -> Int
    func requestString() async -> String
    func tryRequestString() async throws -> String
    func requestIntAndString() async -> (Int, String)
    func tryRequestIntAndString() async throws -> (Int, String)
}

// CHECK-LABEL: sil {{.*}}@{{.*}}15testSlowServing
func testSlowServing(p: SlowServing) async throws {
    // CHECK-NN: bb0([[HOP_TARGET:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C: [[HOP_TARGET:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK: hop_to_executor [[HOP_TARGET]] :
    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Int) -> (), τ_0_0) -> ()
    // CHECK: hop_to_executor [[HOP_TARGET]] :
    let _: Int = await p.requestInt()

    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (NSString) -> (), τ_0_0) -> ()
    // CHECK: hop_to_executor [[HOP_TARGET]] :
    let _: String = await p.requestString()

    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Int, NSString) -> (), τ_0_0) -> ()
    // CHECK: hop_to_executor [[HOP_TARGET]] :
    let _: (Int, String) = await p.requestIntAndString()

    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Int, Optional<NSString>, Optional<NSError>) -> (), τ_0_0) -> ()
    // CHECK: hop_to_executor [[HOP_TARGET]] :
    // CHECK: hop_to_executor [[HOP_TARGET]] :
    // CHECK-NEXT:      builtin "willThrow"
    let _: (Int, String) = try await p.tryRequestIntAndString()
}

// CHECK-LABEL: sil {{.*}}@{{.*}}20testSlowServingAgain
func testSlowServingAgain(p: SlowServing) async throws {
  // CHECK-NN: bb0([[HOP_TARGET:%.*]] : @guaranteed $Optional<any Actor>,
  // CHECK-C: [[HOP_TARGET:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
  // CHECK: hop_to_executor [[HOP_TARGET]] :
  // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Optional<NSString>, Optional<NSError>) -> (), τ_0_0) -> ()
  // CHECK: hop_to_executor [[HOP_TARGET]] :
  // CHECK: hop_to_executor [[HOP_TARGET]] :
  // CHECK-NEXT:      builtin "willThrow"
  let _: String = try await p.tryRequestString()
}

class SlowSwiftServer: NSObject, SlowServing {
    // CHECK-LABEL: sil {{.*}} @$s21objc_async_from_swift15SlowSwiftServerC10requestIntSiyYaF
    // CHECK-NN: bb0([[HOP_TARGET:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C:       [[HOP_TARGET:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[HOP_TARGET]] :
    // CHECK: } // end sil function '$s21objc_async_from_swift15SlowSwiftServerC10requestIntSiyYaF{{.*}}'
    // CHECK-LABEL: sil private {{.*}} @${{.*}}10requestInt{{.*}}To :
    // CHECK:         [[BLOCK_COPY:%.*]] = copy_block %0
    // CHECK:         [[SELF:%.*]] = copy_value %1
    // CHECK:         [[CLOSURE_REF:%.*]] = function_ref [[CLOSURE_IMP:@\$.*10requestInt.*U_To]] :
    // CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_REF]]([[BLOCK_COPY]], [[SELF]])
    // CHECK:         [[RUN_TASK:%.*]] = function_ref @${{.*}}29_runTaskForBridgedAsyncMethod
    // CHECK:         apply [[RUN_TASK]]([[CLOSURE]])
    // CHECK:       sil {{.*}} [[CLOSURE_IMP]]
    // CHECK:         [[BLOCK_COPY:%.*]] = copy_block %0
    // CHECK:         [[NATIVE_RESULT:%.*]] = apply{{.*}}@async
    // CHECK:         [[BLOCK_BORROW:%.*]] = begin_borrow [[BLOCK_COPY]]
    // CHECK:         apply [[BLOCK_BORROW]]([[NATIVE_RESULT]])
    func requestInt() async -> Int { return 0 }
    func requestString() async -> String { return "" }
    // CHECK-LABEL: sil {{.*}} @$s21objc_async_from_swift15SlowSwiftServerC13requestStringSSyYaF
    // CHECK-NN:    bb0([[HOP_TARGET:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C:       [[HOP_TARGET:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[HOP_TARGET]] :
    // CHECK-LABEL: sil {{.*}} @$s21objc_async_from_swift15SlowSwiftServerC16tryRequestStringSSyYaKF
    // CHECK-NN:    bb0([[HOP_TARGET:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C:       [[HOP_TARGET:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[HOP_TARGET]] :
    // CHECK-LABEL: sil shared {{.*}} @${{.*}}16tryRequestString{{.*}}U_To :
    // CHECK:         [[BLOCK_COPY:%.*]] = copy_block %0
    // CHECK:         try_apply{{.*}}@async{{.*}}, normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
    // CHECK:       [[NORMAL]]([[NATIVE_RESULT:%.*]] : @owned $String):
    // CHECK:         [[BLOCK_BORROW:%.*]] = begin_borrow [[BLOCK_COPY]]
    // CHECK:         [[NIL_ERROR:%.*]] = enum $Optional<NSError>, #Optional.none
    // CHECK:         apply [[BLOCK_BORROW]]({{%.*}}, [[NIL_ERROR]])
    // CHECK:       [[ERROR]]([[NATIVE_RESULT:%.*]] : @owned $any Error):
    // CHECK:         [[BLOCK_BORROW:%.*]] = begin_borrow [[BLOCK_COPY]]
    // CHECK:         [[NIL_NSSTRING:%.*]] = enum $Optional<NSString>, #Optional.none
    // CHECK:         apply [[BLOCK_BORROW]]([[NIL_NSSTRING]], {{%.*}})
    func tryRequestString() async throws -> String { return "" }

    // CHECK-LABEL: sil {{.*}} @$s21objc_async_from_swift15SlowSwiftServerC19requestIntAndStringSi_SStyYaF
    // CHECK-NN: bb0([[HOP_TARGET:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C:       [[HOP_TARGET:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[HOP_TARGET]] :
    func requestIntAndString() async -> (Int, String) { return (0, "") }

    // CHECK-LABEL: sil {{.*}} @$s21objc_async_from_swift15SlowSwiftServerC22tryRequestIntAndStringSi_SStyYaKF
    // CHECK-NN: bb0([[HOP_TARGET:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C:       [[HOP_TARGET:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[HOP_TARGET]] :
    func tryRequestIntAndString() async throws -> (Int, String) { return (0, "") }
}


protocol NativelySlowServing {
    func doSomethingSlow(_: String) async -> Int
    func findAnswer() async throws -> String
    func serverRestart(_: String) async
    func findMultipleAnswers() async throws -> (String, Int)
}

extension SlowServer: NativelySlowServing {}

// protocol witness for NativelySlowServing.doSomethingSlow(_:) in conformance SlowServer
//
// CHECK-C-LABEL: sil private [transparent] [thunk] [ossa] @$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP011doSomethingA0ySiSSYaFTW : $@convention(witness_method: NativelySlowServing) @async (@guaranteed String, @in_guaranteed SlowServer) -> Int {
// CHECK-C: bb0([[ARG:%.*]] : @guaranteed $String, [[SELF_ADDR:%.*]] : $*SlowServer):
// CHECK-C:   [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-C:   [[FUNC:%.*]] = function_ref @$sSo10SlowServerC011doSomethingA0ySiSSYaFTO : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServer) -> Int
// CHECK-C:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-C:   hop_to_executor [[ACTOR]]
// CHECK-C:   apply [[FUNC]]([[ACTOR]], [[ARG]], [[SELF]])
// CHECK-C: } // end sil function '$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP011doSomethingA0ySiSSYaFTW'

// CHECK-NN-LABEL: sil private [transparent] [thunk] [ossa] @$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP011doSomethingA0ySiSSYaFTW : $@convention(witness_method: NativelySlowServing) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @in_guaranteed SlowServer) -> Int {
// CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : @guaranteed $String, [[SELF_ADDR:%.*]] : $*SlowServer):
// CHECK-NN:   [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-NN:   [[FUNC:%.*]] = function_ref @$sSo10SlowServerC011doSomethingA0ySiSSYaFTO : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServer) -> Int
// CHECK-NN:   apply [[FUNC]]([[ACTOR]], [[ARG]], [[SELF]])
// CHECK-NN:   hop_to_executor [[ACTOR]]
// CHECK-NN: } // end sil function '$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP011doSomethingA0ySiSSYaFTW'

// protocol witness for NativelySlowServing.findAnswer() in conformance SlowServer
// CHECK-C-LABEL: sil private [transparent] [thunk] [ossa] @$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP10findAnswerSSyYaKFTW : $@convention(witness_method: NativelySlowServing) @async (@in_guaranteed SlowServer) -> (@owned String, @error any Error) {
// CHECK-C: bb0([[SELF_ADDR:%.*]] : $*SlowServer):
// CHECK-C:   [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-C:   [[FUNC:%.*]] = function_ref @$sSo10SlowServerC10findAnswerSSyYaKFTO : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServer) -> (@owned String, @error any Error)
// CHECK-C:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-C:   hop_to_executor [[ACTOR]]
// CHECK-C:   try_apply [[FUNC]]([[ACTOR]], [[SELF]])
// CHECK-C: } // end sil function '$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP10findAnswerSSyYaKFTW'

// CHECK-NN-LABEL: sil private [transparent] [thunk] [ossa] @$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP10findAnswerSSyYaKFTW : $@convention(witness_method: NativelySlowServing) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed SlowServer) -> (@owned String, @error any Error) {
// CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[SELF_ADDR:%.*]] : $*SlowServer):
// CHECK-NN:   [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-NN:   [[FUNC:%.*]] = function_ref @$sSo10SlowServerC10findAnswerSSyYaKFTO : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServer) -> (@owned String, @error any Error)
// CHECK-NN:   try_apply [[FUNC]]([[ACTOR]], [[SELF]])
//
// CHECK-NN: bb1(
// CHECK-NN:    hop_to_executor [[ACTOR]]
//
// CHECK-NN: bb2(
// CHECK-NN:    hop_to_executor [[ACTOR]]
// CHECK-NN: } // end sil function '$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP10findAnswerSSyYaKFTW'

// protocol witness for NativelySlowServing.serverRestart(_:) in conformance SlowServer
// CHECK-C-LABEL: sil private [transparent] [thunk] [ossa] @$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP13serverRestartyySSYaFTW : $@convention(witness_method: NativelySlowServing) @async (@guaranteed String, @in_guaranteed SlowServer) -> () {
// CHECK-C: bb0([[ARG:%.*]] : @guaranteed $String, [[SELF_PTR:%.*]] : $*SlowServer):
// CHECK-C:   [[SELF:%.*]] = load_borrow [[SELF_PTR]]
// CHECK-C:   [[FUNC:%.*]] = function_ref @$sSo10SlowServerC13serverRestartyySSYaFTO : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServer) -> ()
// CHECK-C:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-C:   hop_to_executor [[ACTOR]]
// CHECK-C:   apply [[FUNC]]([[ACTOR]], [[ARG]], [[SELF]])
// CHECK-C: } // end sil function '$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP13serverRestartyySSYaFTW'

// CHECK-NN-LABEL: sil private [transparent] [thunk] [ossa] @$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP13serverRestartyySSYaFTW : $@convention(witness_method: NativelySlowServing) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @in_guaranteed SlowServer) -> () {
// CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : @guaranteed $String, [[SELF_PTR:%.*]] : $*SlowServer):
// CHECK-NN:   [[SELF:%.*]] = load_borrow [[SELF_PTR]]
// CHECK-NN:   [[FUNC:%.*]] = function_ref @$sSo10SlowServerC13serverRestartyySSYaFTO : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServer) -> ()
// CHECK-NN:   apply [[FUNC]]([[ACTOR]], [[ARG]], [[SELF]])
// CHECK-NN:   hop_to_executor [[ACTOR]]
// CHECK-NN: } // end sil function '$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP13serverRestartyySSYaFTW'

// protocol witness for NativelySlowServing.findMultipleAnswers() in conformance SlowServer
// CHECK-C-LABEL: sil private [transparent] [thunk] [ossa] @$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP19findMultipleAnswersSS_SityYaKFTW : $@convention(witness_method: NativelySlowServing) @async (@in_guaranteed SlowServer) -> (@owned String, Int, @error any Error) {
// CHECK-C: bb0([[SELF_ADDR:%.*]] : $*SlowServer):
// CHECK-C:   [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-C:   [[FUNC:%.*]] = function_ref @$sSo10SlowServerC19findMultipleAnswersSS_SityYaKFTO : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServer) -> (@owned String, Int, @error any Error)
// CHECK-C:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK-C:   hop_to_executor [[ACTOR]]
// CHECK-C:   try_apply [[FUNC]]([[ACTOR]], [[SELF]])
// CHECK-C: } // end sil function '$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP19findMultipleAnswersSS_SityYaKFTW'

// CHECK-NN-LABEL: sil private [transparent] [thunk] [ossa] @$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP19findMultipleAnswersSS_SityYaKFTW : $@convention(witness_method: NativelySlowServing) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @in_guaranteed SlowServer) -> (@owned String, Int, @error any Error) {
// CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[SELF_ADDR:%.*]] : $*SlowServer):
// CHECK-NN:   [[SELF:%.*]] = load_borrow [[SELF_ADDR]]
// CHECK-NN:   [[FUNC:%.*]] = function_ref @$sSo10SlowServerC19findMultipleAnswersSS_SityYaKFTO : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServer) -> (@owned String, Int, @error any Error)
// CHECK-NN:   try_apply [[FUNC]]([[ACTOR]], [[SELF]])
//
// CHECK-NN:  bb1(
// CHECK-NN:   hop_to_executor [[ACTOR]]
//
// CHECK-NN:  bb2(
// CHECK-NN:   hop_to_executor [[ACTOR]]
// CHECK-NN: } // end sil function '$sSo10SlowServerC21objc_async_from_swift08NativelyA7ServingA2cDP19findMultipleAnswersSS_SityYaKFTW'

class SlowServerlet: SlowServer {
    // Native Function
    //
    // CHECK-C-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC011doSomethingE8NullablyySiSSYaF : $@convention(method) @async (@guaranteed String, @guaranteed SlowServerlet) -> Int
    // CHECK-C-NOT: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>
    // CHECK-C: [[ACTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
    // CHECK-C:   hop_to_executor [[ACTOR]]
    // CHECK-C:   // end sil function '$s21objc_async_from_swift13SlowServerletC011doSomethingE8NullablyySiSSYaF'

    // CHECK-NN-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC011doSomethingE8NullablyySiSSYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServerlet) -> Int {
    // CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift13SlowServerletC011doSomethingE8NullablyySiSSYaF'

    // TODO: We are labeling this as having caller_isolation_inheriting when NN
    // is enabled... but we do not have an actor parameter. This is incorrect.
    //
    // @objc thunk closure
    //
    // CHECK-LABEL: sil shared [thunk] [ossa] @$s21objc_async_from_swift13SlowServerletC011doSomethingE8NullablyySiSSYaFyyYacfU_To : $@convention(thin) @Sendable @async (NSString, Optional<@convention(block) @Sendable (Int) -> ()>, SlowServerlet) -> () {
    // CHECK-NN: [[NONE:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
    // CHECK: [[STR_ARG:%.*]] = begin_borrow {{.*}} : $String
    // CHECK: [[SELF:%.*]] = begin_borrow {{.*}} : $SlowServerlet
    // CHECK-C: [[FUNC:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC011doSomethingE8NullablyySiSSYaF : $@convention(method) @async (@guaranteed String, @guaranteed SlowServerlet) -> Int
    // CHECK-C: apply [[FUNC]]([[STR_ARG]], [[SELF]])
    // CHECK-NN: [[FUNC:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC011doSomethingE8NullablyySiSSYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServerlet) -> Int
    // CHECK-NN: apply [[FUNC]]([[NONE]], [[STR_ARG]], [[SELF]])
    // CHECK: } // end sil function '$s21objc_async_from_swift13SlowServerletC011doSomethingE8NullablyySiSSYaFyyYacfU_To'
    override func doSomethingSlowNullably(_: String) async -> Int {
        return 0
    }

    // Native function.
    //
    // CHECK-C-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC18findAnswerNullablyyS2SYaF : $@convention(method) @async (@guaranteed String, @guaranteed SlowServerlet) -> @owned String
    // CHECK-C-NOT: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>
    // CHECK-C: [[ACTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
    // CHECK-C-NEXT: hop_to_executor [[ACTOR]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift13SlowServerletC18findAnswerNullablyyS2SYaF'

    // CHECK-NN-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC18findAnswerNullablyyS2SYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServerlet) -> @owned String {
    // CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-NN: hop_to_executor [[ACTOR]]
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift13SlowServerletC18findAnswerNullablyyS2SYaF'

    // @objc closure thunk
    // CHECK-LABEL: sil shared [thunk] [ossa] @$s21objc_async_from_swift13SlowServerletC18findAnswerNullablyyS2SYaFyyYacfU_To : $@convention(thin) @Sendable @async (NSString, Optional<@convention(block) @Sendable (NSString) -> ()>, SlowServerlet) -> () {
    // CHECK-NN: [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
    // CHECK: [[STR_ARG:%.*]] = begin_borrow {{.*}} : $String
    // CHECK: [[SELF:%.*]] = begin_borrow {{.*}} : $SlowServerlet
    // CHECK-C: [[FUNC:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC18findAnswerNullablyyS2SYaF : $@convention(method) @async (@guaranteed String, @guaranteed SlowServerlet) -> @owned String
    // CHECK-C: apply [[FUNC]]([[STR_ARG]], [[SELF]])
    // CHECK-NN: [[FUNC:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC18findAnswerNullablyyS2SYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServerlet) -> @owned String
    // CHECK-NN: apply [[FUNC]]([[ACTOR]], [[STR_ARG]], [[SELF]])
    // CHECK: } // end sil function '$s21objc_async_from_swift13SlowServerletC18findAnswerNullablyyS2SYaFyyYacfU_To'
    override func findAnswerNullably(_ x: String) async -> String {
        return x
    }

    // Native
    //
    // CHECK-C-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC28doSomethingDangerousNullablyyS2SYaKF : $@convention(method) @async (@guaranteed String, @guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-C-NOT: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C: [[ACTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
    // CHECK-C-NEXT: hop_to_executor [[ACTOR]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift13SlowServerletC28doSomethingDangerousNullablyyS2SYaKF'

    // CHECK-NN-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC28doSomethingDangerousNullablyyS2SYaKF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServerlet) -> (@owned String, @error any Error) {
    // CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-NN: hop_to_executor [[ACTOR]]
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift13SlowServerletC28doSomethingDangerousNullablyyS2SYaKF'

    // @objc thunk closure
    //
    // CHECK-LABEL: sil shared [thunk] [ossa] @$s21objc_async_from_swift13SlowServerletC28doSomethingDangerousNullablyyS2SYaKFyyYacfU_To : $@convention(thin) @Sendable @async (NSString, Optional<@convention(block) @Sendable (Optional<NSString>, Optional<NSError>) -> ()>, SlowServerlet) -> () {
    // CHECK-NN: [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
    // CHECK: [[STR_ARG:%.*]] = begin_borrow {{.*}} : $String
    // CHECK: [[SELF:%.*]] = begin_borrow {{.*}} : $SlowServerlet
    // CHECK-C: [[NATIVE:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC28doSomethingDangerousNullablyyS2SYaKF : $@convention(method) @async (@guaranteed String, @guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-C: apply [[NATIVE]]([[STR_ARG]], [[SELF]])
    // CHECK-NN: [[NATIVE:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC28doSomethingDangerousNullablyyS2SYaKF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-NN: apply [[NATIVE]]([[ACTOR]], [[STR_ARG]], [[SELF]])
    // CHECK: } // end sil function '$s21objc_async_from_swift13SlowServerletC28doSomethingDangerousNullablyyS2SYaKFyyYacfU_To'
    override func doSomethingDangerousNullably(_ x: String) async throws -> String {
        return x
    }

    // Native
    //
    // CHECK-C-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC30doSomethingUnspecifiedNullablySSyYaKF : $@convention(method) @async (@guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-C-NOT: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C: [[ACTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
    // CHECK-C-NEXT: hop_to_executor [[ACTOR]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift13SlowServerletC30doSomethingUnspecifiedNullablySSyYaKF'

    // CHECK-NN-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC30doSomethingUnspecifiedNullablySSyYaKF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServerlet) -> (@owned String, @error any Error) {
    // CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift13SlowServerletC30doSomethingUnspecifiedNullablySSyYaKF'

    // @objc closure thunk
    // CHECK-LABEL: sil shared [thunk] [ossa] @$s21objc_async_from_swift13SlowServerletC30doSomethingUnspecifiedNullablySSyYaKFyyYacfU_To : $@convention(thin) @Sendable @async (Optional<@convention(block) @Sendable (Optional<NSString>, Optional<NSError>) -> ()>, SlowServerlet) -> () {
    // CHECK-NN: [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
    // CHECK: [[SELF:%.*]] = begin_borrow {{.*}} : $SlowServerlet
    // CHECK-C: [[NATIVE:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC30doSomethingUnspecifiedNullablySSyYaKF : $@convention(method) @async (@guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-C: try_apply [[NATIVE]]([[SELF]])
    // CHECK-NN: [[NATIVE:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC30doSomethingUnspecifiedNullablySSyYaKF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-NN: try_apply [[NATIVE]]([[ACTOR]], [[SELF]])
    // CHECK: } // end sil function '$s21objc_async_from_swift13SlowServerletC30doSomethingUnspecifiedNullablySSyYaKFyyYacfU_To'
    override func doSomethingUnspecifiedNullably() async throws -> String {
      fatalError()
    }

    // Native
    //
    // CHECK-C-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC17doSomethingFlaggySSyYaKF : $@convention(method) @async (@guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-C-NOT: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C: [[ACTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
    // CHECK-C-NEXT: hop_to_executor [[ACTOR]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift13SlowServerletC17doSomethingFlaggySSyYaKF'

    // CHECK-NN-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC17doSomethingFlaggySSyYaKF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServerlet) -> (@owned String, @error any Error) {
    // CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift13SlowServerletC17doSomethingFlaggySSyYaKF'

    // @objc thunk closure
    // CHECK-LABEL: sil shared [thunk] [ossa] @$s21objc_async_from_swift13SlowServerletC17doSomethingFlaggySSyYaKFyyYacfU_To : $@convention(thin) @Sendable @async (@convention(block) @Sendable ({{.*}}, Optional<NSString>, Optional<NSError>) -> (), SlowServerlet) -> () {
    // CHECK-NN: [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
    // CHECK:    [[SELF:%.*]] = begin_borrow {{.*}} : $SlowServerlet
    // CHECK-C:  [[NATIVE:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC17doSomethingFlaggySSyYaKF : $@convention(method) @async (@guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-C:  try_apply [[NATIVE]]([[SELF]]) : {{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    // CHECK-NN: [[NATIVE:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC17doSomethingFlaggySSyYaKF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-NN: try_apply [[NATIVE]]([[ACTOR]], [[SELF]]) : {{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    //
    // CHECK: [[NORMAL_BB]]({{.*}}):
    // CHECK:    integer_literal {{.*}}0
    //
    // CHECK: [[ERROR_BB]]({{.*}}):
    // CHECK:    integer_literal {{.*}}1
    // CHECK: } // end sil function '$s21objc_async_from_swift13SlowServerletC17doSomethingFlaggySSyYaKFyyYacfU_To'
    override func doSomethingFlaggy() async throws -> String {
        return ""
    }

    // Native
    //
    // CHECK-C-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC21doSomethingZeroFlaggySSyYaKF : $@convention(method) @async (@guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-C-NOT: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C: [[ACTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
    // CHECK-C-NEXT: hop_to_executor [[ACTOR]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift13SlowServerletC21doSomethingZeroFlaggySSyYaKF'
    //
    // CHECK-NN-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC21doSomethingZeroFlaggySSyYaKF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServerlet) -> (@owned String, @error any Error) {
    // CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-NN:  hop_to_executor [[ACTOR]]
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift13SlowServerletC21doSomethingZeroFlaggySSyYaKF'
    //
    // @objc thunk closure
    // CHECK-LABEL: sil shared [thunk] [ossa] @$s21objc_async_from_swift13SlowServerletC21doSomethingZeroFlaggySSyYaKFyyYacfU_To : $@convention(thin) @Sendable @async (@convention(block) @Sendable (Optional<NSString>, {{.*}}, Optional<NSError>) -> (), SlowServerlet) -> () {
    // CHECK-NN:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
    // CHECK:      [[SELF:%.*]] = begin_borrow {{.*}} : $SlowServerlet
    // CHECK-C:    [[NATIVE:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC21doSomethingZeroFlaggySSyYaKF : $@convention(method) @async (@guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-C:    try_apply [[NATIVE]]([[SELF]]) : {{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    // CHECK-NN:   [[NATIVE:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC21doSomethingZeroFlaggySSyYaKF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServerlet) -> (@owned String, @error any Error)
    // CHECK-NN:   try_apply [[NATIVE]]([[ACTOR]], [[SELF]]) : {{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    //
    // CHECK:  [[NORMAL_BB]]({{.*}}):
    // CHECK:      integer_literal {{.*}}1
    //
    // CHECK:  [[ERROR_BB]]({{.*}}):
    // CHECK:      integer_literal {{.*}}0
    override func doSomethingZeroFlaggy() async throws -> String {
        return ""
    }

    // Native
    //
    // CHECK-C-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC28doSomethingMultiResultFlaggySS_SStyYaKF : $@convention(method) @async (@guaranteed SlowServerlet) -> (@owned String, @owned String, @error any Error)
    // CHECK-C-NOT: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-C: [[ACTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
    // CHECK-C-NEXT: hop_to_executor [[ACTOR]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift13SlowServerletC28doSomethingMultiResultFlaggySS_SStyYaKF'
    //
    // CHECK-NN-LABEL: sil hidden [ossa] @$s21objc_async_from_swift13SlowServerletC28doSomethingMultiResultFlaggySS_SStyYaKF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServerlet) -> (@owned String, @owned String, @error any Error) {
    // CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>,
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift13SlowServerletC28doSomethingMultiResultFlaggySS_SStyYaKF'
    //
    // CHECK-LABEL: sil shared [thunk] [ossa] @$s21objc_async_from_swift13SlowServerletC28doSomethingMultiResultFlaggySS_SStyYaKFyyYacfU_To : $@convention(thin) @Sendable @async (@convention(block) @Sendable ({{.*}}, Optional<NSString>, Optional<NSError>, Optional<NSString>) -> (), SlowServerlet) -> () {
    // CHECK-NN: [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
    // CHECK:    [[SELF:%.*]] = begin_borrow {{.*}} : $SlowServerlet
    // CHECK-C:  [[NATIVE:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC28doSomethingMultiResultFlaggySS_SStyYaKF : $@convention(method) @async (@guaranteed SlowServerlet) -> (@owned String, @owned String, @error any Error)
    // CHECK-C:  try_apply [[NATIVE]]([[SELF]]) : {{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    // CHECK-NN: [[NATIVE:%.*]] = function_ref @$s21objc_async_from_swift13SlowServerletC28doSomethingMultiResultFlaggySS_SStyYaKF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed SlowServerlet) -> (@owned String, @owned String, @error any Error)
    // CHECK-NN: try_apply [[NATIVE]]([[ACTOR]], [[SELF]]) : {{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    //
    // CHECK: [[NORMAL_BB]]({{.*}}):
    // CHECK:   integer_literal {{.*}}1
    //
    // CHECK: [[ERROR_BB]]({{.*}}):
    // CHECK:   integer_literal {{.*}}0
    // CHECK: } // end sil function '$s21objc_async_from_swift13SlowServerletC28doSomethingMultiResultFlaggySS_SStyYaKFyyYacfU_To'
    override func doSomethingMultiResultFlaggy() async throws -> (String, String) {
        return ("", "")
    }
}

@globalActor actor FooActor {
    static var shared = FooActor()
}

@FooActor
class ActorConstrained: NSObject {
    // ActorConstrained.foo()
    // CHECK-LABEL: sil hidden [ossa] @$s{{.*}}16ActorConstrainedC3foo{{.*}} : $@convention(method) @async (@guaranteed ActorConstrained) -> Bool {
    // CHECK:         hop_to_executor {{%.*}} : $FooActor

    // @objc ActorConstrained.foo()
    // CHECK-LABEL: sil private [thunk] [ossa] @$s{{.*}}16ActorConstrainedC3foo{{.*}}To : $@convention(objc_method) (@convention(block) (Bool) -> (), ActorConstrained) -> () {
    // CHECK:         [[ASYNC_CLOS:%[0-9]+]] = function_ref @$s{{.*}}16ActorConstrainedC3foo{{.*}}U_To : $@convention(thin) @Sendable @async (@convention(block) (Bool) -> (), ActorConstrained) -> ()
    // CHECK:         [[PRIMED_CLOS:%[0-9]+]] = partial_apply [callee_guaranteed] [[ASYNC_CLOS]](
    // CHECK:         [[TASK_RUNNER:%[0-9]+]] = function_ref @$ss29_runTaskForBridgedAsyncMethodyyyyYaYbcnF
    // CHECK:         apply [[TASK_RUNNER]]([[PRIMED_CLOS]])

    // @objc closure #1 in ActorConstrained.foo()
    // CHECK-LABEL: sil shared [thunk] [ossa] @$s{{.*}}16ActorConstrainedC3foo{{.*}}U_To : $@convention(thin) @Sendable @async (@convention(block) (Bool) -> (), ActorConstrained) -> () {
    // CHECK:           hop_to_executor {{%.*}} : $FooActor
    @objc func foo() async -> Bool {
        return true
    }
}


actor Dril: NSObject {
    // Dril.postTo(twitter:)
    // CHECK-LABEL: sil hidden [ossa] @$s{{.*}}4DrilC6postTo7twitter{{.*}} : $@convention(method) @async (@guaranteed String, @sil_isolated @guaranteed Dril) -> Bool {
    // CHECK:           hop_to_executor {{%.*}} : $Dril

    // @objc Dril.postTo(twitter:)
    // CHECK-LABEL: sil private [thunk] [ossa] @$s{{.*}}4DrilC6postTo7twitter{{.*}}To : $@convention(objc_method) (NSString, @convention(block) (Bool) -> (), @sil_isolated Dril) -> () {
    // CHECK:         [[ASYNC_CLOS:%[0-9]+]] = function_ref @$s{{.*}}4DrilC6postTo7twitter{{.*}}U_To : $@convention(thin) @Sendable @async (NSString, @convention(block) (Bool) -> (), @sil_isolated Dril) -> ()
    // CHECK:         [[PRIMED_CLOS:%[0-9]+]] = partial_apply [callee_guaranteed] [[ASYNC_CLOS]](
    // CHECK:         [[TASK_RUNNER:%[0-9]+]] = function_ref @$ss29_runTaskForBridgedAsyncMethodyyyyYaYbcnF
    // CHECK:         apply [[TASK_RUNNER]]([[PRIMED_CLOS]])
    @objc func postTo(twitter msg: String) async -> Bool {
        return true
    }

    // this is known to not emit a hop in the objc thunk (rdar://80972126)
    @MainActor
    @objc func postFromMainActorTo(twitter msg: String) -> Bool {
        return true
    }
}

// Check that we do not crash here and emit the autoclosures correctly
func testAutoclosureInStaticMethod() {
  final class TestKlass {
    // Default argument for method.
    //
    // CHECK-C-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_ : $@convention(thin) () -> @owned @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error) {
    // CHECK-NN-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_ : $@convention(thin) () -> @owned @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error) {
    //
    // Get standard.
    // CHECK: [[METATYPE:%.*]] = metatype $@objc_metatype SlowServer.Type
    // CHECK: [[GET_STANDARD_FUNC:%.*]] = objc_method %1 : $@objc_metatype SlowServer.Type, #SlowServer.standard!getter.foreign : (SlowServer.Type) -> () -> SlowServer, $@convention(objc_method) (@objc_metatype SlowServer.Type) -> @autoreleased SlowServer
    // CHECK: [[STANDARD:%.*]] = apply [[GET_STANDARD_FUNC]]([[METATYPE]])
    //
    // Then grab value.
    // CHECK-C: [[GET_VALUE:%.*]] = function_ref @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_ : $@convention(thin) (@guaranteed SlowServer) -> @owned @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error)
    // CHECK-NN: [[GET_VALUE:%.*]] = function_ref @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_ : $@convention(thin) (@guaranteed SlowServer) -> @owned @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error)
    // CHECK: [[RESULT:%.*]] = apply [[GET_VALUE]]([[STANDARD]])
    //
    // Then we need to thunk to eliminate the implicit leading parameter. We use
    // the thunk that passes in .none so this acts as a concurrent function.
    //
    // CHECK-C: [[THUNK_FN:%.*]] = function_ref @$sScA_pSgS2Ss5Error_pIegHgILgozo_S2SsAB_pIegHgozo_TR : $@convention(thin) @async (@guaranteed String, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error)) -> (@owned String, @error any Error)
    // CHECK-C: [[THUNKED:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]([[RESULT]])
    // CHECK-C: return [[THUNKED]]
    // CHECK-NN: return [[RESULT]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_'
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_'

    // This is the first implicit closure. We close over self here.
    //
    // CHECK-C-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_ : $@convention(thin) (@guaranteed SlowServer) -> @owned @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error) {
    // CHECK: bb0([[SELF:%.*]] :
    //
    // Close over self and return it.
    // CHECK-C:   [[SECOND_CLOSURE:%.*]] = function_ref @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_S2SYaKYCcfu0_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServer) -> (@owned String, @error any Error)
    // CHECK-NN:  [[SECOND_CLOSURE:%.*]] = function_ref @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_S2SYaKYCcfu0_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServer) -> (@owned String, @error any Error)
    // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
    // CHECK:   [[CLOSE_OVER_SELF:%.*]] = partial_apply [callee_guaranteed] [[SECOND_CLOSURE]]([[SELF_COPY]])
    // CHECK:   return [[CLOSE_OVER_SELF]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_'
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_' 

    // The second closure. In this function we actually perform the objective-c call.
    // CHECK-C-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_S2SYaKYCcfu0_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServer) -> (@owned String, @error any Error) {
    // CHECK-NN-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_S2SYaKYCcfu0_ : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed SlowServer) -> (@owned String, @error any Error) {
    // CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : @guaranteed $String, [[CAPTURE:%.*]] : @closureCapture @guaranteed $SlowServer):
    //
    // Hop to the actor
    // CHECK:   hop_to_executor [[ACTOR]]
    //
    // Declare result
    // CHECK:   [[RESULT:%.*]] = alloc_stack $String
    //
    // Bridge string arg.
    // CHECK:   [[ARG_C:%.*]] = copy_value [[ARG]]
    // CHECK:   [[BRIDGE_FN:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF : $@convention(method) (@guaranteed String) -> @owned NSString
    // CHECK:   [[ARG_C_B:%.*]] = begin_borrow [[ARG_C]]
    // CHECK:   [[NS_STRING:%.*]] = apply [[BRIDGE_FN]]([[ARG_C_B]])
    //
    // ObjC Method
    // CHECK:   [[OBJC_METHOD:%.*]] = objc_method [[CAPTURE]]
    //
    // Prepare the continuation.
    // CHECK:   [[RAW_UNSAFE_CONT:%.*]] = get_async_continuation_addr [throws] String, [[RESULT]]
    // CHECK:   [[UNSAFE_CONT:%.*]] = struct $UnsafeContinuation<String, any Error> ([[RAW_UNSAFE_CONT]] :
    // CHECK:   [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage Any
    // CHECK:   [[PROJ_BLOCK_STORAGE:%.*]] = project_block_storage [[BLOCK_STORAGE]]
    // CHECK:   [[INIT_PROJ_BLOCK_STORAGE:%.*]] = init_existential_addr [[PROJ_BLOCK_STORAGE]]
    // CHECK:   store [[UNSAFE_CONT]] to [trivial] [[INIT_PROJ_BLOCK_STORAGE]]
    // CHECK:   merge_isolation_region [[BLOCK_STORAGE]] : $*@block_storage Any, [[RESULT]]
    // CHECK:   [[OBJC_COMPLETION_HANDLER_IMPL:%.*]] = function_ref @$sSo8NSStringCSgSo7NSErrorCSgIeyBhyy_SSTz_ : $@convention(c) @Sendable (@inout_aliasable @block_storage Any, Optional<NSString>, Optional<NSError>) -> ()
    // CHECK:   [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage Any, invoke [[OBJC_COMPLETION_HANDLER_IMPL]]
    // CHECK:   merge_isolation_region [[CAPTURE]] : $SlowServer, [[BLOCK_STORAGE]]
    // CHECK:   apply [[OBJC_METHOD]]([[NS_STRING]], [[BLOCK]], [[CAPTURE]])
    // CHECK:   await_async_continuation [[RAW_UNSAFE_CONT]] : $Builtin.RawUnsafeContinuation, resume [[RESUME_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    //
    // CHECK: [[RESUME_BB]]:
    // CHECK:   hop_to_executor [[ACTOR]]
    // CHECK:   [[LOADED_RESULT:%.*]] = load [take] [[RESULT]]
    // CHECK:   return [[LOADED_RESULT]]
    //
    // CHECK: [[ERROR_BB]]([[ERROR:%.*]] :
    // CHECK:   hop_to_executor [[ACTOR]]
    // CHECK:   throw [[ERROR]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_S2SYaKYCcfu0_'
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKYCcSo10SlowServerCcfu_S2SYaKYCcfu0_'

      // thunk for @escaping @callee_guaranteed @async (@guaranteed Actor?, @guaranteed String) -> (@owned String, @error @owned Error)

    // CHECK-C-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sScA_pSgS2Ss5Error_pIegHgILgozo_S2SsAB_pIegHgozo_TR : $@convention(thin) @async (@guaranteed String, @guaranteed @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error)) -> (@owned String, @error any Error) {
    // CHECK-C: bb0([[ARG:%.*]] : @guaranteed $String, [[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error)):
    // CHECK-C:   [[ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
    // CHECK-C:   hop_to_executor [[ACTOR]]
    // CHECK-C:   try_apply [[FUNC]]([[ACTOR]], [[ARG]])
    // CHECK-C: } // end sil function '$sScA_pSgS2Ss5Error_pIegHgILgozo_S2SsAB_pIegHgozo_TR'

    // Actual static method
    //
    // We do not actually care about FileChecking this... we just need to
    // FileCheck the hop_to_executor to show to FileCheck that we expect these
    // hop_to_executor lines to occur in the function since we reject
    // non-explicitly specified hop_to_executor lines.
    //
    // CHECK-C-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZ : $@convention(method) @async (@guaranteed String, @guaranteed @noescape @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error), @thick TestKlass.Type) -> @owned Optional<String> {
    // CHECK-C: bb0([[STRING:%.*]] : @guaranteed $String, [[COMPLETION:%.*]] : @guaranteed $@noescape @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error), [[METATYPE:%.*]] : $@thick TestKlass.Type)
    // CHECK-C:   [[EXEC_NONE:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
    // CHECK-C:   hop_to_executor [[EXEC_NONE]]
    // CHECK-C:   hop_to_executor [[EXEC_NONE]]
    // CHECK-C:   hop_to_executor [[EXEC_NONE]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZ'

    // CHECK-NN-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZ : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error), @thick TestKlass.Type) -> @owned Optional<String> {
    // CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[STRING:%.*]] : @guaranteed $String, [[COMPLETION:%.*]] : @guaranteed $@noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error), %3 : $@thick TestKlass.Type):
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C8getValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZ'

    static func getValue(id: String,
                         valueForKey: (_ identifier: String) async throws -> String = SlowServer.standard.value(withKey:)) async -> String? {
      let result: String
      do {
        result = try await valueForKey(id)
      } catch {
        return nil
      }
      return result
    }

    // Default argument for method.
    //
    // CHECK-C-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_ : $@convention(thin) () -> @owned @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error) {
    // CHECK-NN-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_ : $@convention(thin) () -> @owned @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error) {
    //
    // Get standard.
    // CHECK: [[METATYPE:%.*]] = metatype $@objc_metatype SlowServer.Type
    // CHECK: [[GET_STANDARD_FUNC:%.*]] = objc_method %1 : $@objc_metatype SlowServer.Type, #SlowServer.standard!getter.foreign : (SlowServer.Type) -> () -> SlowServer, $@convention(objc_method) (@objc_metatype SlowServer.Type) -> @autoreleased SlowServer
    // CHECK: [[STANDARD:%.*]] = apply [[GET_STANDARD_FUNC]]([[METATYPE]])
    //
    // Then grab value.
    // CHECK-C: [[GET_VALUE:%.*]] = function_ref @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_ : $@convention(thin) (@guaranteed SlowServer) -> @owned @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error)
    // CHECK-NN: [[GET_VALUE:%.*]] = function_ref @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_ : $@convention(thin) (@guaranteed SlowServer) -> @owned @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error)
    // CHECK: [[RESULT:%.*]] = apply [[GET_VALUE]]([[STANDARD]])
    //
    // CHECK-NN: [[THUNK:%.*]] = function_ref @$sS2Ss5Error_pIegHgozo_ScA_pSgS2SsAA_pIegHgILgozo_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error)) -> (@owned String, @error any Error)
    // CHECK-NN: [[THUNKED_RESULT:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[RESULT]])
    // CHECK-C: return [[RESULT]]
    // CHECK-NN: return [[THUNKED_RESULT]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_'
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_'

    // This is the first implicit closure. We close over self here.
    //
    // CHECK-C-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_ : $@convention(thin) (@guaranteed SlowServer) -> @owned @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error) {
    // CHECK-NN-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_ : $@convention(thin) (@guaranteed SlowServer) -> @owned @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error) {
    // CHECK: bb0([[SELF:%.*]] :
    //
    // Close over self and return it.
    // CHECK-C:  [[SECOND_CLOSURE:%.*]] = function_ref @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_S2SYaKScMYccfu0_ : $@convention(thin) @async (@guaranteed String, @guaranteed SlowServer) -> (@owned String, @error any Error)
    // CHECK-NN: [[SECOND_CLOSURE:%.*]] = function_ref @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_S2SYaKScMYccfu0_ : $@convention(thin) @async (@guaranteed String, @guaranteed SlowServer) -> (@owned String, @error any Error)
    // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
    // CHECK:   [[CLOSE_OVER_SELF:%.*]] = partial_apply [callee_guaranteed] [[SECOND_CLOSURE]]([[SELF_COPY]])
    // CHECK:   return [[CLOSE_OVER_SELF]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_'
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_'

    // The second closure. In this function we actually perform the objective-c call.
    //
    // It is main actor isolated.
    //
    // CHECK-C-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_S2SYaKScMYccfu0_ : $@convention(thin) @async (@guaranteed String, @guaranteed SlowServer) -> (@owned String, @error any Error) {
    // CHECK-NN-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_S2SYaKScMYccfu0_ : $@convention(thin) @async (@guaranteed String, @guaranteed SlowServer) -> (@owned String, @error any Error) {
    // CHECK: bb0([[ARG:%.*]] : @guaranteed $String, [[CAPTURE:%.*]] : @closureCapture @guaranteed $SlowServer):
    //
    // CHECK:   [[ACTOR:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
    //
    // Hop to the actor
    // CHECK:   [[ACTOR_B:%.*]] = begin_borrow [[ACTOR]]
    // CHECK:   hop_to_executor [[ACTOR_B]]
    //
    // Declare result
    // CHECK:   [[RESULT:%.*]] = alloc_stack $String
    //
    // Bridge string arg.
    // CHECK:   [[ARG_C:%.*]] = copy_value [[ARG]]
    // CHECK:   [[BRIDGE_FN:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF : $@convention(method) (@guaranteed String) -> @owned NSString
    // CHECK:   [[ARG_C_B:%.*]] = begin_borrow [[ARG_C]]
    // CHECK:   [[NS_STRING:%.*]] = apply [[BRIDGE_FN]]([[ARG_C_B]])
    //
    // ObjC Method
    // CHECK:   [[OBJC_METHOD:%.*]] = objc_method [[CAPTURE]]
    //
    // Prepare the continuation.
    // CHECK:   [[RAW_UNSAFE_CONT:%.*]] = get_async_continuation_addr [throws] String, [[RESULT]]
    // CHECK:   [[UNSAFE_CONT:%.*]] = struct $UnsafeContinuation<String, any Error> ([[RAW_UNSAFE_CONT]] :
    // CHECK:   [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage Any
    // CHECK:   [[PROJ_BLOCK_STORAGE:%.*]] = project_block_storage [[BLOCK_STORAGE]]
    // CHECK:   [[INIT_PROJ_BLOCK_STORAGE:%.*]] = init_existential_addr [[PROJ_BLOCK_STORAGE]]
    // CHECK:   store [[UNSAFE_CONT]] to [trivial] [[INIT_PROJ_BLOCK_STORAGE]]
    // CHECK:   merge_isolation_region [[BLOCK_STORAGE]] : $*@block_storage Any, [[RESULT]]
    // CHECK:   [[OBJC_COMPLETION_HANDLER_IMPL:%.*]] = function_ref @$sSo8NSStringCSgSo7NSErrorCSgIeyByy_SSTz_ : $@convention(c) (@inout_aliasable @block_storage Any, Optional<NSString>, Optional<NSError>) -> ()
    // CHECK:   [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage Any, invoke [[OBJC_COMPLETION_HANDLER_IMPL]]
    // CHECK:   apply [[OBJC_METHOD]]([[NS_STRING]], [[BLOCK]], [[CAPTURE]])
    // CHECK:   await_async_continuation [[RAW_UNSAFE_CONT]] : $Builtin.RawUnsafeContinuation, resume [[RESUME_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    //
    // CHECK: [[RESUME_BB]]:
    // CHECK:   hop_to_executor [[ACTOR_B]]
    // CHECK:   [[LOADED_RESULT:%.*]] = load [take] [[RESULT]]
    // CHECK:   return [[LOADED_RESULT]]
    //
    // CHECK: [[ERROR_BB]]([[ERROR:%.*]] :
    // CHECK:   hop_to_executor [[ACTOR_B]]
    // CHECK:   throw [[ERROR]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_S2SYaKScMYccfu0_'
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZfA0_S2SYaKScMYccSo10SlowServerCcfu_S2SYaKScMYccfu0_'

    // NOTE: This is earlier in the file when not compiling with
    // nonisolated(nonsending) by default just due to the way the compiler emits
    // thunks at different times due to different usages.
    //
    // CHECK-NN-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sS2Ss5Error_pIegHgozo_ScA_pSgS2SsAA_pIegHgILgozo_TR : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error)) -> (@owned String, @error any Error) {
    // CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : @guaranteed $String, [[FUNC:%.*]] : @guaranteed $@async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error)):
    // CHECK-NN:   try_apply [[FUNC]]([[ARG]])
    //
    // CHECK-NN: bb1(
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    //
    // CHECK-NN: bb2(
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN: } // end sil function '$sS2Ss5Error_pIegHgozo_ScA_pSgS2SsAA_pIegHgILgozo_TR'

    // Actual static method
    //
    // This is not main actor isolated
    //
    // CHECK-C-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZ : $@convention(method) @async (@guaranteed String, @guaranteed @noescape @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error), @thick TestKlass.Type) -> @owned Optional<String> {
    // CHECK-C: bb0([[STRING:%.*]] : @guaranteed $String, [[COMPLETION:%.*]] : @guaranteed $@noescape @async @callee_guaranteed (@guaranteed String) -> (@owned String, @error any Error), [[METATYPE:%.*]] : $@thick TestKlass.Type)
    // CHECK-C:   [[EXEC_NONE:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
    // CHECK-C:   hop_to_executor [[EXEC_NONE]]
    // CHECK-C:   hop_to_executor [[EXEC_NONE]]
    // CHECK-C:   hop_to_executor [[EXEC_NONE]]
    // CHECK-C: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKXEtYaFZ'
    //
    // CHECK-NN-LABEL: sil private [ossa] @$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZ : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error), @thick TestKlass.Type) -> @owned Optional<String> {
    // CHECK-NN: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[STRING:%.*]] : @guaranteed $String, [[COMPLETION:%.*]] : @guaranteed $@noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed String) -> (@owned String, @error any Error), %3 : $@thick TestKlass.Type)
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN:   hop_to_executor [[ACTOR]]
    // CHECK-NN: } // end sil function '$s21objc_async_from_swift29testAutoclosureInStaticMethodyyF9TestKlassL_C17getMainActorValue2id11valueForKeySSSgSS_S2SYaKYCXEtYaFZ'
    static func getMainActorValue(id: String,
                                  valueForKey: (_ identifier: String) async throws -> String = SlowServer.standard.mainActorValue(withKey:)) async -> String? {
      let result: String
      do {
        result = try await valueForKey(id)
      } catch {
        return nil
      }
      return result
    }
  }
}
