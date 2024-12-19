// RUN: %target-swift-frontend -swift-version 6 -enable-experimental-feature GenerateForceToMainActorThunks -import-objc-header %S/Inputs/hoptomainactorifneeded.h -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 6 -enable-experimental-feature GenerateForceToMainActorThunks -import-objc-header %S/Inputs/hoptomainactorifneeded.h -Xllvm -sil-print-types -emit-lowered-sil %s | %FileCheck -check-prefix=LOWERED %s

// READ THIS: This test validates that basic lowering of hop to main actor if
// needed works. For fuller tests that validate that things actually hop, see
// hoptomainactorifneeded_interpreter.

// REQUIRES: objc_interop
// REQUIRES: swift_feature_GenerateForceToMainActorThunks

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
// LOWERED-LABEL: sil shared [thunk] @$s22hoptomainactorifneeded11testClosureyyYaFTTH : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> () {
// LOWERED: bb0([[ARG:%.*]] : $@callee_guaranteed () -> ()):
// LOWERED:   [[FUNC:%.*]] = function_ref @$ss19_taskRunOnMainActor9operationyyyScMYcc_tF : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> ()
// LOWERED:   apply [[FUNC]]([[ARG]])
// LOWERED: } // end sil function '$s22hoptomainactorifneeded11testClosureyyYaFTTH

// testClosure.
// LOWERED-LABEL: sil hidden @$s22hoptomainactorifneeded11testClosureyyYaF : $@convention(thin) @async () -> () {
// LOWERED:   [[CLOSURE:%.*]] = function_ref @$s22hoptomainactorifneeded11testClosureyyYaFyyScMYccfU_ : $@convention(thin) () -> ()
// LOWERED:   [[THICK_CLOSURE:%.*]] = thin_to_thick_function [[CLOSURE]]
// LOWERED:   [[THUNK:%.*]] = function_ref @$s22hoptomainactorifneeded11testClosureyyYaFTTH : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> ()
// LOWERED:   [[THUNKED_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[THICK_CLOSURE]])
// LOWERED:   [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
// LOWERED:   [[PROJECT_BLOCK_STORAGE:%.*]] = project_block_storage [[BLOCK_STORAGE]]
// LOWERED:   store [[THUNKED_CLOSURE]] to [[PROJECT_BLOCK_STORAGE]]
// LOWERED:   [[THUNK_THUNK_CALLEE:%.*]] = function_ref @$sIeg_IeyB_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed () -> ()) -> ()
// LOWERED:   [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage @callee_guaranteed () -> (), invoke [[THUNK_THUNK_CALLEE]]
// LOWERED:   [[FINISHED_BLOCK:%.*]] = copy_block [[BLOCK]]
// LOWERED:   [[OPT_BLOCK:%.*]] = enum $Optional<@convention(block) () -> ()>, #Optional.some!enumelt, [[FINISHED_BLOCK]]
// LOWERED:   [[USE_CLOSURE_CALLEE:%.*]] = function_ref @useClosure : $@convention(c) (Optional<@convention(block) () -> ()>) -> ()
// LOWERED:   apply [[USE_CLOSURE_CALLEE]]([[OPT_BLOCK]])
// LOWERED: } // end sil function '$s22hoptomainactorifneeded11testClosureyyYaF'
@MainActor
func testClosure() async {
  useClosure {
  }
}

await testClosure()
