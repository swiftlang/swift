// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -I %S/Inputs/custom-modules -enable-experimental-concurrency %s -verify | %FileCheck %s
// REQUIRES: objc_interop

import Foundation
import ObjCConcurrency

// CHECK-LABEL: sil {{.*}}@${{.*}}14testSlowServer
func testSlowServer(slowServer: SlowServer) async throws {
  // CHECK: [[RESUME_BUF:%.*]] = alloc_stack $Int
  // CHECK: [[METHOD:%.*]] = objc_method {{.*}} $@convention(objc_method) (NSString, @convention(block) (Int) -> (), SlowServer) -> ()
  // CHECK: [[CONT:%.*]] = get_async_continuation_addr $Int, [[RESUME_BUF]]
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage UnsafeContinuation<Int>
  // CHECK: [[CONT_SLOT:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: store [[CONT]] to [[CONT_SLOT]]
  // CHECK: [[BLOCK_IMPL:%.*]] = %20 = function_ref @{{.*}} : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<Int>, Int) -> ()
  // CHECK: [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] {{.*}}, invoke [[BLOCK_IMPL]]
  // CHECK: apply [[METHOD]]({{.*}}, [[BLOCK]], %0)
  // CHECK: await_async_continuation [[CONT]] {{.*}}, resume [[RESUME:bb[0-9]+]]
  // CHECK: [[RESUME]]:
  // CHECK: [[RESULT:%.*]] = load [trivial] [[RESUME_BUF]]
  // CHECK: dealloc_stack [[RESUME_BUF]]
  let _: Int = await slowServer.doSomethingSlow("mail")

  // CHECK: [[RESUME_BUF:%.*]] = alloc_stack $Optional<String>
  // CHECK: [[METHOD:%.*]] = objc_method {{.*}} $@convention(objc_method) (@convention(block) (Optional<NSString>, Optional<NSError>) -> (), SlowServer) -> ()
  // CHECK: [[CONT:%.*]] = get_async_continuation_addr $Optional<String>, [[RESUME_BUF]]
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage UnsafeContinuation<Optional<String>>
  // CHECK: [[CONT_SLOT:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: store [[CONT]] to [[CONT_SLOT]]
  // CHECK: [[BLOCK_IMPL:%.*]] = %20 = function_ref @{{.*}} : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<Optional<String>>, Optional<NSString>, Optional<NSError>) -> ()
  // CHECK: [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] {{.*}}, invoke [[BLOCK_IMPL]]
  // CHECK: apply [[METHOD]]({{.*}}, [[BLOCK]], %0)
  // CHECK: await_async_continuation [[CONT]] {{.*}}, resume [[RESUME:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
  // CHECK: [[RESUME]]:
  // CHECK: [[RESULT:%.*]] = load [take] [[RESUME_BUF]]
  // CHECK: destroy_value [[RESULT]]
  // CHECK: dealloc_stack [[RESUME_BUF]]
  // CHECK: [[ERROR]]([[ERROR_VALUE:%.*]] : @owned $Error):
  // CHECK: dealloc_stack [[RESUME_BUF]]
  // CHECK: throw [[ERROR_VALUE]]

  let _: String? = try await slowServer.findAnswer()

  // CHECK: objc_method {{.*}} $@convention(objc_method) (NSString, @convention(block) () -> (), SlowServer) -> ()
  await slowServer.serverRestart("somewhere")
}
