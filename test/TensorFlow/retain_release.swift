// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify | %FileCheck %s
import TensorFlow

// Unit tests on generating balanced retain/release SIL instructions.

public func test3Adds() {
  let a = Tensor([1]).toDevice()
  let b = Tensor([2]).toDevice()
  let c = Tensor([3]).toDevice()
  let _ = a + b + c
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test3Adds{{.*}}
// CHECK: sil @{{.*}}test3Adds{{.*}} : $@convention(thin) () -> () {
//
// TThese 2 retains are to prepare for the first a + a.
// CHECK: strong_retain [[Ha:%.*]] : $TensorHandle<Int>
// CHECK: strong_retain [[Hb:%.*]] : $TensorHandle<Int>
//
// We're passing 3 TensorHandle's into the StartTensorComputation call.
// CHECK: alloc_stack $(OpaquePointer, OpaquePointer, OpaquePointer)
// CHECK: function_ref @_swift_tfc_StartTensorComputation
//
// Compiler generates these 2 releases to balance the above 2 retains.
// CHECK: strong_release [[Ha]] : $TensorHandle<Int>
// CHECK: strong_release [[Hb]] : $TensorHandle<Int>
//
// For the tensor handle to c, compiler has cancelled out the pair of retain and
// release. There should be no more retain instructions.
// CHECK-NOT: strong_retain {{.*}} : $TensorHandle<Int>
//
// CHECK: function_ref @_swift_tfc_FinishTensorComputation
//
// These final releases balances the original instructions that generated the
// handles.
// CHECK: strong_release {{.*}} : $TensorHandle<Int>
// CHECK: strong_release {{.*}} : $TensorHandle<Int>
// CHECK: strong_release {{.*}} : $TensorHandle<Int>

public func testAddsWithIntermediateTensorSingleUse() {
  let a = Tensor([1]).toDevice()
  let _ = a + a + a
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testAddsWithIntermediateTensorSingleUse{{.*}}
// CHECK: sil @{{.*}}testAddsWithIntermediateTensorSingleUse{{.*}} : $@convention(thin) () -> () {
//
// CHECK: [[H:%.*]] = struct_extract {{.*}} : $Tensor<Int>, #Tensor.handle
//
// TThese 2 retains are to prepare for the first a + a.
// CHECK: strong_retain [[H]] : $TensorHandle<Int>
// CHECK: strong_retain [[H]] : $TensorHandle<Int>
//
// We're passing 1 TensorHandle into the StartTensorComputation call.
// CHECK: alloc_stack $OpaquePointer
// CHECK: function_ref @_swift_tfc_StartTensorComputation
//
// Compiler generates these 2 releases to balance the above 2 retains.
// CHECK: strong_release [[H]] : $TensorHandle<Int>
// CHECK: strong_release [[H]] : $TensorHandle<Int>
//
// For the input arg c to the second add, compiler has cancelled out the pair of
// retain and release. There should be no more retain instructions.
// CHECK-NOT: strong_retain [[H]] : $TensorHandle<Int>
//
// CHECK: function_ref @_swift_tfc_FinishTensorComputation
//
// This final release balances the original instruction that generated H.
// CHECK: strong_release [[H]] : $TensorHandle<Int>

public func testAddsWithIntermediateTensorMultiUses() {
  let a = Tensor([1]).toDevice()
  let tmp1 = a + a
  let tmp2 = tmp1 + a
  let _ = tmp1 + tmp2
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testAddsWithIntermediateTensorMultiUses{{.*}}
// CHECK: sil @{{.*}}testAddsWithIntermediateTensorMultiUses{{.*}} : $@convention(thin) () -> () {
//
// CHECK: [[H:%.*]] = struct_extract {{.*}} : $Tensor<Int>, #Tensor.handle
//
// TThese 2 retains are to prepare for a + a.
// CHECK: strong_retain [[H]] : $TensorHandle<Int>
// CHECK: strong_retain [[H]] : $TensorHandle<Int>
//
// We're passing 1 TensorHandle into the StartTensorComputation call.
// CHECK: alloc_stack $OpaquePointer
// CHECK: function_ref @_swift_tfc_StartTensorComputation
//
// Compiler generates these 2 releases to balance the above 2 retains.
// CHECK: strong_release [[H]] : $TensorHandle<Int>
// CHECK: strong_release [[H]] : $TensorHandle<Int>
//
// No more retain instructions.
// CHECK-NOT: strong_retain [[H]] : $TensorHandle<Int>
//
// CHECK: function_ref @_swift_tfc_FinishTensorComputation
//
// This final release balances the original instruction that generated H.
// CHECK: strong_release [[H]] : $TensorHandle<Int>
