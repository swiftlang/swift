// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -I %S/Inputs/custom-modules  -disable-availability-checking %s -verify | %FileCheck --implicit-check-not=hop_to_executor --check-prefix=CHECK --check-prefix=CHECK-%target-cpu %s
// REQUIRES: concurrency
// REQUIRES: objc_interop

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
    // CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK: hop_to_executor [[GENERIC_EXECUTOR]] :
    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Int) -> (), τ_0_0) -> ()
    // CHECK: hop_to_executor [[GENERIC_EXECUTOR]] :
    let _: Int = await p.requestInt()

    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (NSString) -> (), τ_0_0) -> ()
    // CHECK: hop_to_executor [[GENERIC_EXECUTOR]] :
    let _: String = await p.requestString()

    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Int, NSString) -> (), τ_0_0) -> ()
    // CHECK: hop_to_executor [[GENERIC_EXECUTOR]] :
    let _: (Int, String) = await p.requestIntAndString()

    // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Int, Optional<NSString>, Optional<NSError>) -> (), τ_0_0) -> ()
    // CHECK: hop_to_executor [[GENERIC_EXECUTOR]] :
    // CHECK: hop_to_executor [[GENERIC_EXECUTOR]] :
    // CHECK-NEXT:      builtin "willThrow"
    let _: (Int, String) = try await p.tryRequestIntAndString()
}

// CHECK-LABEL: sil {{.*}}@{{.*}}20testSlowServingAgain
func testSlowServingAgain(p: SlowServing) async throws {
  // CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
  // CHECK: hop_to_executor [[GENERIC_EXECUTOR]] :
  // CHECK: objc_method {{.*}} $@convention(objc_method) <τ_0_0 where τ_0_0 : SlowServing> (@convention(block) (Optional<NSString>, Optional<NSError>) -> (), τ_0_0) -> ()
  // CHECK: hop_to_executor [[GENERIC_EXECUTOR]] :
  // CHECK: hop_to_executor [[GENERIC_EXECUTOR]] :
  // CHECK-NEXT:      builtin "willThrow"
  let _: String = try await p.tryRequestString()
}

class SlowSwiftServer: NSObject, SlowServing {
    // CHECK-LABEL: sil {{.*}} @$s21objc_async_from_swift15SlowSwiftServerC10requestIntSiyYaF
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
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
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
    // CHECK-LABEL: sil {{.*}} @$s21objc_async_from_swift15SlowSwiftServerC16tryRequestStringSSyYaKF
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
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
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
    func requestIntAndString() async -> (Int, String) { return (0, "") }

    // CHECK-LABEL: sil {{.*}} @$s21objc_async_from_swift15SlowSwiftServerC22tryRequestIntAndStringSi_SStyYaKF
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
    func tryRequestIntAndString() async throws -> (Int, String) { return (0, "") }
}


protocol NativelySlowServing {
    func doSomethingSlow(_: String) async -> Int
    func findAnswer() async throws -> String
    func serverRestart(_: String) async
    func findMultipleAnswers() async throws -> (String, Int)
}

extension SlowServer: NativelySlowServing {}

class SlowServerlet: SlowServer {
    // CHECK-LABEL: sil{{.*}}13SlowServerlet{{.*}}011doSomethingE8Nullably{{.*}}
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
    override func doSomethingSlowNullably(_: String) async -> Int {
        return 0
    }

    // CHECK-LABEL: sil{{.*}}13SlowServerlet{{.*}}18findAnswerNullably{{.*}}
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
    override func findAnswerNullably(_ x: String) async -> String {
        return x
    }

    // CHECK-LABEL: sil{{.*}}13SlowServerlet{{.*}}28doSomethingDangerousNullably{{.*}} :
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
    override func doSomethingDangerousNullably(_ x: String) async throws -> String {
        return x
    }

    // CHECK-LABEL: sil{{.*}}13SlowServerlet{{.*}}30doSomethingUnspecifiedNullably{{.*}} :
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
    override func doSomethingUnspecifiedNullably() async throws -> String {
      fatalError()
    }

    // CHECK-LABEL: sil{{.*}}13SlowServerlet{{.*}}17doSomethingFlaggy{{.*}} :
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
    // CHECK-LABEL: sil private{{.*}}13SlowServerlet{{.*}}17doSomethingFlaggy{{.*}}To :
    // CHECK:         try_apply{{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    // CHECK:       [[NORMAL_BB]]({{.*}}):
    // CHECK:         integer_literal {{.*}}0 
    // CHECK:       [[ERROR_BB]]({{.*}}):
    // CHECK:         integer_literal {{.*}}1 
    override func doSomethingFlaggy() async throws -> String {
        return ""
    }

    // CHECK-LABEL: sil{{.*}}13SlowServerlet{{.*}}21doSomethingZeroFlaggy{{.*}} :
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
    // CHECK-LABEL: sil private{{.*}}13SlowServerlet{{.*}}21doSomethingZeroFlaggy{{.*}}To :
    // CHECK:         try_apply{{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    // CHECK:       [[NORMAL_BB]]({{.*}}):
    // CHECK:         integer_literal {{.*}}1 
    // CHECK:       [[ERROR_BB]]({{.*}}):
    // CHECK:         integer_literal {{.*}}0 
    override func doSomethingZeroFlaggy() async throws -> String {
        return ""
    }

    // CHECK-LABEL: sil{{.*}}13SlowServerlet{{.*}}28doSomethingMultiResultFlaggy{{.*}} :
    // CHECK:         [[GENERIC_EXECUTOR:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
    // CHECK:         hop_to_executor [[GENERIC_EXECUTOR]] :
    // CHECK-LABEL: sil private{{.*}}13SlowServerlet{{.*}}28doSomethingMultiResultFlaggy{{.*}}To :
    // CHECK:         try_apply{{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    // CHECK:       [[NORMAL_BB]]({{.*}}):
    // CHECK:         integer_literal {{.*}}1 
    // CHECK:       [[ERROR_BB]]({{.*}}):
    // CHECK:         integer_literal {{.*}}0 
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
    // CHECK-LABEL: sil hidden [ossa] @$s{{.*}}4DrilC6postTo7twitter{{.*}} : $@convention(method) @async (@guaranteed String, @isolated @guaranteed Dril) -> Bool {
    // CHECK:           hop_to_executor {{%.*}} : $Dril

    // @objc Dril.postTo(twitter:)
    // CHECK-LABEL: sil private [thunk] [ossa] @$s{{.*}}4DrilC6postTo7twitter{{.*}}To : $@convention(objc_method) (NSString, @convention(block) (Bool) -> (), @isolated Dril) -> () {
    // CHECK:         [[ASYNC_CLOS:%[0-9]+]] = function_ref @$s{{.*}}4DrilC6postTo7twitter{{.*}}U_To : $@convention(thin) @Sendable @async (NSString, @convention(block) (Bool) -> (), @isolated Dril) -> ()
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
