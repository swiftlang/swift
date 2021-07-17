// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -disable-availability-checking -I %S/Inputs/custom-modules %s -verify | %FileCheck --enable-var-scope --check-prefix=CHECK --check-prefix=CHECK-%target-cpu %s
// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation
import EffectfulProperties

// CHECK-LABEL: sil {{.*}}@${{.*}}13testJustAsync
func testJustAsync(eff : EffProps) async {
  // CHECK: [[RESUME_BUF:%.*]] = alloc_stack $NSObject
  // CHECK: [[METHOD:%.*]] = objc_method {{.*}} $@convention(objc_method) (@convention(block) (NSObject) -> (), EffProps) -> ()
  // CHECK: [[CONT:%.*]] = get_async_continuation_addr NSObject, [[RESUME_BUF]]
  // CHECK: [[WRAPPED:%.*]] = struct $UnsafeContinuation<NSObject, Never> ([[CONT]] : $Builtin.RawUnsafeContinuation)
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage UnsafeContinuation<NSObject, Never>
  // CHECK: [[CONT_SLOT:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: store [[WRAPPED]] to [trivial] [[CONT_SLOT]]
  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[NSO_COMPLETION_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<NSObject, Never>, NSObject) -> ()
  // CHECK: [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] {{.*}}, invoke [[BLOCK_IMPL]]
  // CHECK: apply [[METHOD]]([[BLOCK]], %0)
  // CHECK: await_async_continuation [[CONT]] {{.*}}, resume [[RESUME:bb[0-9]+]]

  // CHECK: [[RESUME]]:
  // CHECK: [[RESULT:%.*]] = load [take] [[RESUME_BUF]]
  // CHECK: dealloc_stack [[RESUME_BUF]]
  let _ = await eff.doggo
}

// CHECK-LABEL: sil {{.*}}@${{.*}}15testAsyncThrows
func testAsyncThrows(eff : EffProps) async {
  // CHECK: [[RESUME_BUF:%.*]] = alloc_stack $Optional<NSObject>
  // CHECK: [[METHOD:%.*]] = objc_method {{.*}} $@convention(objc_method) (@convention(block) (Optional<NSObject>, Optional<NSError>) -> (), EffProps) -> ()
  // CHECK: [[CONT:%.*]] = get_async_continuation_addr [throws] Optional<NSObject>, [[RESUME_BUF]]
  // CHECK: [[WRAPPED:%.*]] = struct $UnsafeContinuation<Optional<NSObject>, Error> ([[CONT]] : $Builtin.RawUnsafeContinuation)
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage UnsafeContinuation<Optional<NSObject>, Error>
  // CHECK: [[CONT_SLOT:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: store [[WRAPPED]] to [trivial] [[CONT_SLOT]]
  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[NSO_COMPLETION_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage UnsafeContinuation<Optional<NSObject>, Error>, Optional<NSObject>, Optional<NSError>) -> ()
  // CHECK: [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] {{.*}}, invoke [[BLOCK_IMPL]]
  // CHECK: apply [[METHOD]]([[BLOCK]], %0)
  // CHECK: await_async_continuation [[CONT]] {{.*}}, resume [[RESUME:bb[0-9]+]], error [[RESUME_ERROR:bb[0-9]+]]

  // CHECK: [[RESUME]]:
  // CHECK: [[RESULT:%.*]] = load [take] [[RESUME_BUF]]
  // CHECK: dealloc_stack [[RESUME_BUF]]

  // CHECK: [[RESUME_ERROR]]([[ERROR_VAL:%[0-9]+]] : @owned $Error):
  // CHECK: dealloc_stack [[RESUME_BUF]]
  let _ = try? await eff.catto
}

// CHECK-LABEL: sil {{.*}}@${{.*}}17testMainActorProp
func testMainActorProp(eff : EffProps) async {
  // CHECK-NOT: hop_to_executor
  // CHECK: } // end sil function '${{.*}}17testMainActorProp
  let _ = await eff.mainDogProp
}

// CHECK-LABEL: sil {{.*}}@${{.*}}19testMainActorMethod
func testMainActorMethod(eff : EffProps) async {
  // CHECK-NOT: hop_to_executor
  // CHECK: } // end sil function '${{.*}}19testMainActorMethod
  let _ = await eff.regularMainDog()
}
