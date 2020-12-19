// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -I %S/Inputs/custom-modules -enable-experimental-concurrency %s -verify | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-%target-cpu %s
// REQUIRES: objc_interop

import Foundation
import ObjCConcurrency

// CHECK-LABEL: sil {{.*}}@${{.*}}14testSlowServer
func testSlowServer(slowServer: SlowServer) async throws {
  // CHECK: [[RESUME_BUF:%.*]] = alloc_stack $Int
  // CHECK: [[METHOD:%.*]] = objc_method {{.*}} $@convention(objc_method) (NSString, @convention(block) (Int) -> (), SlowServer) -> ()
  // CHECK: [[CONT:%.*]] = get_async_continuation_addr Int, [[RESUME_BUF]]
  // CHECK: [[WRAPPED:%.*]] = struct $UnsafeContinuation<Int> ([[CONT]] : $Builtin.RawUnsafeContinuation)
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage UnsafeContinuation<Int>
  // CHECK: [[CONT_SLOT:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: store [[WRAPPED]] to [trivial] [[CONT_SLOT]]
  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[INT_COMPLETION_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<Int>, Int) -> ()
  // CHECK: [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] {{.*}}, invoke [[BLOCK_IMPL]]
  // CHECK: apply [[METHOD]]({{.*}}, [[BLOCK]], %0)
  // CHECK: await_async_continuation [[CONT]] {{.*}}, resume [[RESUME:bb[0-9]+]]
  // CHECK: [[RESUME]]:
  // CHECK: [[RESULT:%.*]] = load [trivial] [[RESUME_BUF]]
  // CHECK: dealloc_stack [[RESUME_BUF]]
  let _: Int = await slowServer.doSomethingSlow("mail")

  let _: Int = await slowServer.doSomethingSlowNullably("mail")

  // CHECK: [[RESUME_BUF:%.*]] = alloc_stack $String
  // CHECK: [[METHOD:%.*]] = objc_method {{.*}} $@convention(objc_method) (@convention(block) (Optional<NSString>, Optional<NSError>) -> (), SlowServer) -> ()
  // CHECK: [[CONT:%.*]] = get_async_continuation_addr [throws] String, [[RESUME_BUF]]
  // CHECK: [[WRAPPED:%.*]] = struct $UnsafeThrowingContinuation<String> ([[CONT]] : $Builtin.RawUnsafeContinuation)
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage UnsafeThrowingContinuation<String>
  // CHECK: [[CONT_SLOT:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: store [[WRAPPED]] to [trivial] [[CONT_SLOT]]
  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[STRING_COMPLETION_THROW_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeThrowingContinuation<String>, Optional<NSString>, Optional<NSError>) -> ()
  // CHECK: [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] {{.*}}, invoke [[BLOCK_IMPL]]
  // CHECK: apply [[METHOD]]([[BLOCK]], %0)
  // CHECK: await_async_continuation [[CONT]] {{.*}}, resume [[RESUME:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
  // CHECK: [[RESUME]]:
  // CHECK: [[RESULT:%.*]] = load [take] [[RESUME_BUF]]
  // CHECK: destroy_value [[RESULT]]
  // CHECK: dealloc_stack [[RESUME_BUF]]
  let _: String = await try slowServer.findAnswer()

  // CHECK: objc_method {{.*}} $@convention(objc_method) (NSString, @convention(block) () -> (), SlowServer) -> ()
  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[VOID_COMPLETION_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<()>) -> ()
  await slowServer.serverRestart("somewhere")

  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[NSSTRING_INT_THROW_COMPLETION_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeThrowingContinuation<(String, Int)>, Optional<NSString>, Int, Optional<NSError>) -> ()
  let (_, _): (String, Int) = await try slowServer.findMultipleAnswers()

  let (_, _): (Bool, Bool) = await try slowServer.findDifferentlyFlavoredBooleans()

  // CHECK: [[ERROR]]([[ERROR_VALUE:%.*]] : @owned $Error):
  // CHECK:   dealloc_stack [[RESUME_BUF]]
  // CHECK:   br [[THROWBB:bb[0-9]+]]([[ERROR_VALUE]]
  // CHECK: [[THROWBB]]([[ERROR_VALUE:%.*]] : @owned $Error):
  // CHECK:   throw [[ERROR_VALUE]]

  let _: String = await slowServer.findAnswerNullably("foo")
  let _: String = await try slowServer.doSomethingDangerousNullably("foo")
}

// CHECK: sil{{.*}}@[[INT_COMPLETION_BLOCK]]
// CHECK:   [[CONT_ADDR:%.*]] = project_block_storage %0
// CHECK:   [[CONT:%.*]] = load [trivial] [[CONT_ADDR]]
// CHECK:   [[RESULT_BUF:%.*]] = alloc_stack $Int
// CHECK:   store %1 to [trivial] [[RESULT_BUF]]
// CHECK:   [[RESUME:%.*]] = function_ref @{{.*}}resumeUnsafeContinuation
// CHECK:   apply [[RESUME]]<Int>([[CONT]], [[RESULT_BUF]])

// CHECK: sil{{.*}}@[[STRING_COMPLETION_THROW_BLOCK]]
// CHECK:   [[RESUME_IN:%.*]] = copy_value %1
// CHECK:   [[ERROR_IN:%.*]] = copy_value %2
// CHECK:   [[CONT_ADDR:%.*]] = project_block_storage %0
// CHECK:   [[CONT:%.*]] = load [trivial] [[CONT_ADDR]]
// CHECK:   [[ERROR_IN_B:%.*]] = begin_borrow [[ERROR_IN]]
// CHECK:   switch_enum [[ERROR_IN_B]] : {{.*}}, case #Optional.some!enumelt: [[ERROR_BB:bb[0-9]+]], case #Optional.none!enumelt: [[RESUME_BB:bb[0-9]+]]
// CHECK: [[RESUME_BB]]:
// CHECK:   [[RESULT_BUF:%.*]] = alloc_stack $String
// CHECK:   [[BRIDGE:%.*]] = function_ref @{{.*}}unconditionallyBridgeFromObjectiveC
// CHECK:   [[BRIDGED_RESULT:%.*]] = apply [[BRIDGE]]([[RESUME_IN]]
// CHECK:   store [[BRIDGED_RESULT]] to [init] [[RESULT_BUF]]
// CHECK:   [[RESUME:%.*]] = function_ref @{{.*}}resumeUnsafeThrowingContinuation
// CHECK:   apply [[RESUME]]<String>([[CONT]], [[RESULT_BUF]])
// CHECK:   br [[END_BB:bb[0-9]+]]
// CHECK: [[END_BB]]:
// CHECK:   return
// CHECK: [[ERROR_BB]]([[ERROR_IN_UNWRAPPED:%.*]] : @guaranteed $NSError):
// CHECK:   [[ERROR:%.*]] = init_existential_ref [[ERROR_IN_UNWRAPPED]]
// CHECK:   [[RESUME_WITH_ERROR:%.*]] = function_ref @{{.*}}resumeUnsafeThrowingContinuationWithError
// CHECK:   [[ERROR_COPY:%.*]] = copy_value [[ERROR]]
// CHECK:   apply [[RESUME_WITH_ERROR]]<String>([[CONT]], [[ERROR_COPY]])
// CHECK:   br [[END_BB]]

// CHECK: sil {{.*}} @[[VOID_COMPLETION_BLOCK]]
// CHECK:   [[CONT_ADDR:%.*]] = project_block_storage %0
// CHECK:   [[CONT:%.*]] = load [trivial] [[CONT_ADDR]]
// CHECK:   [[RESULT_BUF:%.*]] = alloc_stack $()
// CHECK:   [[RESUME:%.*]] = function_ref @{{.*}}resumeUnsafeContinuation
// CHECK:   apply [[RESUME]]<()>([[CONT]], [[RESULT_BUF]])

// CHECK: sil{{.*}}@[[NSSTRING_INT_THROW_COMPLETION_BLOCK]]
// CHECK:   [[RESULT_BUF:%.*]] = alloc_stack $(String, Int)
// CHECK:   [[RESULT_0_BUF:%.*]] = tuple_element_addr [[RESULT_BUF]] {{.*}}, 0
// CHECK:   [[BRIDGE:%.*]] = function_ref @{{.*}}unconditionallyBridgeFromObjectiveC
// CHECK:   [[BRIDGED:%.*]] = apply [[BRIDGE]]
// CHECK:   store [[BRIDGED]] to [init] [[RESULT_0_BUF]]
// CHECK:   [[RESULT_1_BUF:%.*]] = tuple_element_addr [[RESULT_BUF]] {{.*}}, 1
// CHECK:   store %2 to [trivial] [[RESULT_1_BUF]]
