// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -emit-silgen -checked-async-objc-bridging=off -target %target-swift-5.1-abi-triple -I %S/Inputs/custom-modules %s -verify | %FileCheck --enable-var-scope --check-prefix=CHECK --check-prefix=CHECK-%target-cpu %s
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
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage Any
  // CHECK: [[CONT_SLOT:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: [[CONT_SLOT_ADDR:%.*]] = init_existential_addr [[CONT_SLOT]]
  // CHECK: store [[WRAPPED]] to [trivial] [[CONT_SLOT_ADDR]]
  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[NSO_COMPLETION_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage Any, NSObject) -> ()
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
  // CHECK: [[WRAPPED:%.*]] = struct $UnsafeContinuation<Optional<NSObject>, any Error> ([[CONT]] : $Builtin.RawUnsafeContinuation)
  // CHECK: [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage Any
  // CHECK: [[CONT_SLOT:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK: [[CONT_SLOT_ADDR:%.*]] = init_existential_addr [[CONT_SLOT]]
  // CHECK: store [[WRAPPED]] to [trivial] [[CONT_SLOT_ADDR]]
  // CHECK: [[BLOCK_IMPL:%.*]] = function_ref @[[NSO_COMPLETION_BLOCK:.*]] : $@convention(c) (@inout_aliasable @block_storage Any, Optional<NSObject>, Optional<NSError>) -> ()
  // CHECK: [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] {{.*}}, invoke [[BLOCK_IMPL]]
  // CHECK: apply [[METHOD]]([[BLOCK]], %0)
  // CHECK: await_async_continuation [[CONT]] {{.*}}, resume [[RESUME:bb[0-9]+]], error [[RESUME_ERROR:bb[0-9]+]]

  // CHECK: [[RESUME]]:
  // CHECK: [[RESULT:%.*]] = load [take] [[RESUME_BUF]]
  // CHECK: dealloc_stack [[RESUME_BUF]]

  // CHECK: [[RESUME_ERROR]]([[ERROR_VAL:%[0-9]+]] : @owned $any Error):
  // CHECK: dealloc_stack [[RESUME_BUF]]
  let _ = try? await eff.catto
}

// CHECK-LABEL: sil {{.*}}@${{.*}}17testMainActorProp
func testMainActorProp(eff : EffProps) async {
  // CHECK:         [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
  // CHECK-NEXT:    hop_to_executor [[GENERIC_EXEC]] :
  // CHECK:         hop_to_executor [[GENERIC_EXEC]] :
  // CHECK: } // end sil function '${{.*}}17testMainActorProp
  let _ = await eff.mainDogProp
}

// CHECK-LABEL: sil {{.*}}@${{.*}}19testMainActorMethod
func testMainActorMethod(eff : EffProps) async {
  // CHECK:         [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none
  // CHECK-NEXT:    hop_to_executor [[GENERIC_EXEC]] :
  // CHECK:         hop_to_executor [[GENERIC_EXEC]] :
  // CHECK: } // end sil function '${{.*}}19testMainActorMethod
  let _ = await eff.regularMainDog()
}
