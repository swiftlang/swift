// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -o -
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify | %FileCheck %s

import TensorFlow

// Unit tests on generating balanced retain/release SIL instructions.

public func test3Adds(x: Tensor<Int32>, y: Tensor<Int32>, z: Tensor<Int32>) {
  let a = x.toAccelerator()
  let b = y.toAccelerator()
  let c = z.toAccelerator()
  let _ = a + b + c
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test3Adds{{.*}}
// CHECK: @{{.*}}test3Adds{{.*}} : $@convention(thin)

// CHECK-NOT: retain
// CHECK-NOT: release

// Some possible retain/releases are emitted by the deabstraction pass, so we do
// not check on them.
// CHECK: [[X:%.*]] = struct_extract %0 : $Tensor<Int32>, #Tensor.handle
// CHECK: [[Y:%.*]] = struct_extract %1 : $Tensor<Int32>, #Tensor.handle
// CHECK: [[Z:%.*]] = struct_extract %2 : $Tensor<Int32>, #Tensor.handle
//
// We're passing 3 TensorHandle's into the StartTensorComputation call.
// CHECK: alloc_stack $(OpaquePointer, OpaquePointer, OpaquePointer)
// CHECK: function_ref @_swift_tfc_StartTensorComputation
//
// CHECK: function_ref @_swift_tfc_FinishTensorComputation
//
// These final releases balances the original instructions that generated the
// handles.
// CHECK: strong_release {{.*}} : $TensorHandle<Int32>
// CHECK: strong_release {{.*}} : $TensorHandle<Int32>
// CHECK: strong_release {{.*}} : $TensorHandle<Int32>
// CHECK-LABEL: ---


public func testAddsWithIntermediateTensorSingleUse(x: Tensor<Int32>) {
  let a = x.toAccelerator()
  let _ = a + a + a
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testAddsWithIntermediateTensorSingleUse{{.*}}
// CHECK: @{{.*}}testAddsWithIntermediateTensorSingleUse{{.*}} : $@convention(thin) (@guaranteed Tensor<Int32>) -> () {
//
// CHECK: [[H:%.*]] = struct_extract {{.*}} : $Tensor<Int32>, #Tensor.handle
//
// We're passing 1 TensorHandle into the StartTensorComputation call.
// CHECK: alloc_stack $OpaquePointer
// CHECK: function_ref @_swift_tfc_StartTensorComputation
//
// For the input arg c to the second add, compiler has cancelled out the pair of
// retain and release.
//
// CHECK: function_ref @_swift_tfc_FinishTensorComputation
//
// This final release balances the original instruction that generated H.
// CHECK: strong_release [[H]] : $TensorHandle<Int32>
// CHECK-LABEL: ---

public func testAddsWithIntermediateTensorMultiUses(x: Tensor<Int32>) {
  let a = x.toAccelerator()
  let tmp1 = a + a
  let tmp2 = tmp1 + a
  let _ = tmp1 + tmp2
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testAddsWithIntermediateTensorMultiUses{{.*}}
// CHECK: @{{.*}}testAddsWithIntermediateTensorMultiUses{{.*}} : $@convention(thin)
//
// CHECK: [[H:%.*]] = struct_extract {{.*}} : $Tensor<Int32>, #Tensor.handle
//
// We're passing 1 TensorHandle into the StartTensorComputation call.
// CHECK: alloc_stack $OpaquePointer
// CHECK: function_ref @_swift_tfc_StartTensorComputation
//
// CHECK: function_ref @_swift_tfc_FinishTensorComputation
//
// This final release balances the original instruction that generated H.
// CHECK: strong_release [[H]] : $TensorHandle<Int32>
// CHECK-LABEL: ---


public func testBalancedRetainReleases() {
  let t1 = Tensor<Float>(1.2)
  let _ = t1 + t1
  let _ = t1.array
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testBalancedRetainReleases{{.*}}
// CHECK: sil @{{.*}}testBalancedRetainReleases{{.*}} : $@convention(thin)
//
// CHECK: function_ref @_swift_tfc_FinishTensorComputation
// CHECK: [[H:%.*]] = alloc_ref $TensorHandle<Float>
//
// __tf_to_host is called here
// CHECK: [[TOHOST:%.*]] = function_ref @__tf_to_host
//
// Currently we generate a retain for the use of apply below
// CHECK: strong_retain [[H]] : $TensorHandle<Float>
// CHECK: apply [[TOHOST]]<Float>([[H]])
//
// CHECK: strong_release [[H]] : $TensorHandle<Float>
// CHECK: strong_release [[H]] : $TensorHandle<Float>
// CHECK-LABEL: ---
