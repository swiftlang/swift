// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -o -
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify | %FileCheck %s
import TensorFlow

// Unit tests on generating balanced retain/release SIL instructions.

public func test3Adds(x: Tensor<Int>, y: Tensor<Int>, z: Tensor<Int>) {
  let a = x.toDevice()
  let b = y.toDevice()
  let c = z.toDevice()
  let _ = a + b + c
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test3Adds{{.*}}
// CHECK: sil [thunk] [always_inline] @{{.*}}test3Adds{{.*}} : $@convention(thin) (@owned Tensor<Int>, @owned Tensor<Int>, @owned Tensor<Int>) -> () {
//
// These 2 retains are to prepare for the first a + a.
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

public func testAddsWithIntermediateTensorSingleUse(x: Tensor<Int>) {
  let a = x.toDevice()
  let _ = a + a + a
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testAddsWithIntermediateTensorSingleUse{{.*}}
// CHECK: sil [thunk] [always_inline] @{{.*}}testAddsWithIntermediateTensorSingleUse{{.*}} : $@convention(thin) (@owned Tensor<Int>) -> () {
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

public func testAddsWithIntermediateTensorMultiUses(x: Tensor<Int>) {
  let a = x.toDevice()
  let tmp1 = a + a
  let tmp2 = tmp1 + a
  let _ = tmp1 + tmp2
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testAddsWithIntermediateTensorMultiUses{{.*}}
// CHECK: sil [thunk] [always_inline] @{{.*}}testAddsWithIntermediateTensorMultiUses{{.*}} : $@convention(thin)
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
