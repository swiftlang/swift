// RUN: %target-swift-frontend -swift-version 6 -enable-experimental-feature GenerateForceToMainActorThunks -import-objc-header %S/Inputs/hoptomainactorifneeded.h -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 6 -enable-experimental-feature GenerateForceToMainActorThunks -import-objc-header %S/Inputs/hoptomainactorifneeded.h -emit-ir %s | %FileCheck -check-prefix=IR %s

// READ THIS: This test validates that basic lowering of hop to main actor if
// needed works. For fuller tests that validate that things actually hop, see
// hoptomainactorifneeded_interpreter.

// REQUIRES: objc_interop
// REQUIRES: asserts

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// CHECK-LABEL: // testClosure()
// CHECK: // Isolation: global_actor. type: MainActor
// CHECK: sil hidden [ossa] @$s22hoptomainactorifneeded11testClosureyyYaF : $@convention(thin) @async () -> () {
// CHECK:   [[FUNC:%.*]] = function_ref @$s22hoptomainactorifneeded11testClosureyyYaFyyScMYccfU_ : $@convention(thin) () -> ()
// CHECK:   [[TTFI:%.*]] = thin_to_thick_function [[FUNC]] : $@convention(thin) () -> () to $@callee_guaranteed () -> ()
// CHECK:   [[THUNKED:%.*]] = thunk [hop_to_mainactor_if_needed] [[TTFI]]()
// CHECK:   [[BLOCK:%.*]] = alloc_stack $@block_storage @callee_guaranteed () -> () // users: %16, %12, %9
// CHECK:   [[BLOCK_PROJECT:%.*]] = project_block_storage [[BLOCK]] : $*@block_storage @callee_guaranteed () -> () // users: %15, %10
// CHECK:   store [[THUNKED]] to [init] [[BLOCK_PROJECT]] : $*@callee_guaranteed () -> () // id: %10
// CHECK: } // end sil function '$s22hoptomainactorifneeded11testClosureyyYaF'

// Check that we actually emit the thunk and call taskRunOnMainActor.
//
// IR-LABEL: define linkonce_odr hidden swiftcc void @"$s22hoptomainactorifneeded11testClosureyyYaFTTH"(ptr %0, ptr %1)
// IR-NEXT: entry:
// IR-NEXT:   call swiftcc void @"$ss19_taskRunOnMainActor9operationyyyScMYcc_tF"(ptr %0, ptr %1)
// IR-NEXT:   ret void
// IR-NEXT: }

// Test Closure. We just hop onto the main actor.
// IR-LABEL: define hidden swifttailcc void @"$s22hoptomainactorifneeded11testClosureyyYaF"(ptr swiftasync %0)
// IR: musttail call swifttailcc void @swift_task_switch(ptr swiftasync {{%.*}}, ptr @"$s22hoptomainactorifneeded11testClosureyyYaFTY0_",

// After we hop onto the main actor, we store the partial apply forwarder to the
// hop to main actor closure.
//
// IR: define internal swifttailcc void @"$s22hoptomainactorifneeded11testClosureyyYaFTY0_"(ptr swiftasync [[ASYNC_CONTEXT:%.*]])
// IR: %async.ctx.frameptr = getelementptr inbounds i8, ptr [[ASYNC_CONTEXT]], i32 16
// IR: [[FUNC1:%.*]] = getelementptr inbounds %"$s22hoptomainactorifneeded11testClosureyyYaF.Frame", ptr %async.ctx.frameptr, i32 0, i32 0
// IR: [[FUNC2:%.*]] = getelementptr inbounds { %objc_block, %swift.function }, ptr [[FUNC1]], i32 0, i32 1
// IR: [[FUNC3:%.*]] = getelementptr inbounds %swift.function, ptr [[FUNC2]], i32 0, i32 0
// IR: store ptr @"$s22hoptomainactorifneeded11testClosureyyYaFTTHTA", ptr [[FUNC3]]

// In the partial apply forwarder, we need to call the actual hop to main actor
// thunk.
//
// IR: define internal swiftcc void @"$s22hoptomainactorifneeded11testClosureyyYaFTTHTA"(ptr swiftself [[FRAME:%.*]])
// IR-NEXT: entry:
// IR-NEXT: [[FRAME_GEP:%.*]] = getelementptr inbounds <{ %swift.refcounted, %swift.function }>, ptr [[FRAME]], i32 0, i32 1
// IR-NEXT: [[FUNC2:%.*]] = getelementptr inbounds %swift.function, ptr [[FRAME_GEP]], i32 0, i32 0
// IR-NEXT: [[FUNC2_LOADED:%.*]] = load ptr, ptr [[FUNC2]]
// IR-NEXT: [[DATA:%.*]] = getelementptr inbounds %swift.function, ptr [[FRAME_GEP]]
// IR-NEXT: [[DATA_LOADED:%.*]] = load ptr, ptr [[DATA]]
// IR-NEXT: tail call swiftcc void @"$s22hoptomainactorifneeded11testClosureyyYaFTTH"(ptr [[FUNC2_LOADED]], ptr [[DATA_LOADED]])
// IR-NEXT: ret void
// IR-NEXT: }

@MainActor
func testClosure() async {
  useClosure {
  }
}

await testClosure()
