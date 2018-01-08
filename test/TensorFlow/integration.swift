// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify | %FileCheck %s
import TensorFlow

public func testTensor() {
  var x = Tensor<Float>([1.0, 2.0, 3.0])  // expected-warning {{value implicitly copied to accelerator, use .toDevice() to make transfer explicit}}
  x += x
  x -= x  // expected-warning {{value implicitly copied to the host, use .toHost() to make transfer explicit}}
  // GraphGen doesn't support sends yet: expected-error @-1 {{internal error generating TensorFlow graph}}

  print(x) // expected-note {{value used here}}
  var y = Tensor1D<Float>(1, 2, 3.0).toDevice()
  y += y
  print(y)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testTensor{{.*}}
// CHECK:  sil private @{{.*}}testTensor{{.*}} : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK-NEXT:   %1 = builtin "__tfop_Add__tt:t__"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %2 = builtin "__tfop_Sub__tt:t__"(%1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %3 = builtin "tensorflowSend_1"<TensorHandle<Float>>(%2 : $TensorHandle<Float>) : $()
// CHECK-NEXT:   %4 = builtin "tensorflowReceive_0"<TensorHandle<Float>>() : $TensorHandle<Float>
// CHECK-NEXT:   %5 = builtin "__tfop_Add__tt:t__"(%4 : $TensorHandle<Float>, %4 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   return %5 : $TensorHandle<Float>


// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testTensor{{.*}}
// CHECK: sil @{{.*}}testTensor{{.*}} : $@convention(thin) () -> () {

// Graph lowering fails on testTensor because it requires send and receive instructions.
// CHECK: string_literal utf8 ""
// CHECK-NEXT:  integer_literal $Builtin.Int64, 0
// CHECK-NOT: = apply

// We're passing one TensorHandle in.
// CHECK: [[ALLOC:%.*]] = alloc_stack $OpaquePointer
// CHECK: ref_element_addr
// CHECK: begin_access [read] [static] [[ALLOC]] : $*OpaquePointer
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorProgram
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN:%.*]](
// CHECK: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorProgram
// CHECK-NEXT: apply [[FINISHFN]]([[PROGRAM]],

public func testScalar(f: Float) {
  var x = Tensor<Float>(f) +    // expected-warning {{value implicitly copied to accelerator}}
          Tensor<Float>(1.0)
  x += x
  print(x)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testScalar{{.*}}
// CHECK: sil private @{{.*}}testScalar{{.*}} : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK-NEXT:   %1 = metatype $@thick Float.Type
// CHECK-NEXT:   %2 = float_literal $Builtin.FPIEEE32, 0x3F800000 // 1
// CHECK-NEXT:   %3 = builtin "__tfop_Const__dc:t__"(%1 : $@thick Float.Type, %2 : $Builtin.FPIEEE32) : $TensorHandle<Float>
// CHECK-NEXT:   %4 = builtin "__tfop_Add__tt:t__"(%0 : $TensorHandle<Float>, %3 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %5 = builtin "__tfop_Add__tt:t__"(%4 : $TensorHandle<Float>, %4 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   return %5 : $TensorHandle<Float>
// CHECK-NEXT: }


// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testScalar{{.*}}
// CHECK: sil @{{.*}}testScalar{{.*}} : $@convention(thin) (Float) -> () {

// Graph lowering succeeds on this function
// CHECK: string_literal utf8 "{{.....}}
// CHECK-NEXT:  integer_literal $Builtin.Int64, {{[1-9]}}

// StartTensorProgram is called with one input tensor
// CHECK: [[TENSORS:%.*]] = struct $UnsafePointer<OpaquePointer> ({{%.*}} : $Builtin.RawPointer)
// CHECK-NEXT: [[TENSOR_COUNT:%.*]] = integer_literal $Builtin.Int64, 1
// CHECK-NEXT: [[TENSOR_COUNT_STRUCT:%.*]] = struct $Int ([[TENSOR_COUNT]] : $Builtin.Int64)
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorProgram
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN]]({{%.*}}, {{%.*}}, [[TENSORS]], [[TENSOR_COUNT_STRUCT]]
// CHECK: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorProgram
// CHECK-NEXT: apply [[FINISHFN]]([[PROGRAM]],


public func testExitBranch(i : Int) {
  var x = Tensor<Float>(1.0)

  if i == 0 {
    return   // Should terminate the tensor program.
  }

  x += x
  print(x)
}

// The tensor program should have no branch.

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testExitBranch{{.*}}
// CHECK: sil private @{{.*}}testExitBranch{{.*}} : $@callee_owned () -> TensorHandle<Float> {
// CHECK: bb0:
// CHECK-NEXT:   %0 = metatype $@thick Float.Type
// CHECK-NEXT:   %1 = float_literal $Builtin.FPIEEE32, 0x3F800000 // 1
// CHECK-NEXT:   %2 = builtin "__tfop_Const__dc:t__"(%0 : $@thick Float.Type, %1 : $Builtin.FPIEEE32) : $TensorHandle<Float>
// CHECK-NEXT:   %3 = builtin "__tfop_Add__tt:t__"(%2 : $TensorHandle<Float>, %2 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   return %3 : $TensorHandle<Float>
// CHECK-NEXT: }


// The host program should kill the tensor program if the early exit happens,
// and finish it on the normal path.

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testExitBranch{{.*}}
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorProgram
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN]](
// CHECK: cond_br

// CHECK: bb1:
// CHECK: [[TERMFN:%.*]] = function_ref @_swift_tfc_TerminateTensorProgram
// CHECK-NEXT: apply [[TERMFN]]([[PROGRAM]]) : $@convention(thin) (@owned TensorProgram) -> ()
// CHECK: br bb3

// CHECK: bb2:
// CHECK: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorProgram
// CHECK-NEXT: apply [[FINISHFN]]([[PROGRAM]],

// CHECK: bb3:



// This program results in a boolean parameter being passed in.
public func test_bool_param(cond: Bool) {  // expected-error {{TFLowerGraph can only handle single basic block programs}}
  var a = Tensor1D<Float>(1,2,3).toDevice()
  let b = Tensor1D<Float>(1,2,4).toDevice()

  if cond {  // expected-warning {{value implicitly copied to accelerator}}
    a -= b
  }
  a += b
  print(a.toHost())
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test_bool_param{{.*}}
// CHECK: sil private @{{.*}}test_bool_param{{.*}} : $@callee_owned (TensorHandle<Builtin.Int1>, TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float>
// CHECK: bb0(%0 : $TensorHandle<Builtin.Int1>, %1 : $TensorHandle<Float>, %2 : $TensorHandle<Float>):
// CHECK: %3 = builtin "tf_tensor_to_i1"(%0 : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
// CHECK: cond_br %3, bb2, bb1


// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test_bool_param{{.*}}
// CHECK: = function_ref @_swift_tfc_CreateCTensorHandle : $@convention(thin)
// CHECK-NEXT: = integer_literal $Builtin.Int32, 10
// CHECK-NEXT: = struct $UInt32 ({{.*}} : $Builtin.Int32)
// CHECK-NEXT:  = struct $TF_DataType ({{.*}} : $UInt32)
// CHECK-NEXT:  = alloc_stack $Builtin.Int1
// CHECK-NEXT: store
// CHECK-NEXT:  = begin_access [read] [static]
// CHECK-NEXT:  = apply {{.*}}<Builtin.Int1>({{.*}}, {{.*}}) : $@convention(thin)
// CHECK-NEXT:  end_access
// CHECK-NEXT:  dealloc_stack


// This should also result in a boolean parameter, but due to evaluation order
// we get a send instead.
public func test_bool_param2(cond: Bool) {  // expected-error {{TFLowerGraph can only handle single basic block programs}}
  var a = Tensor1D<Float>(1,2,3).toDevice()
  let b = Tensor1D<Float>(1,2,4).toDevice()

  a += b

  if cond {  // expected-warning {{value implicitly copied to accelerator}}
    a -= b
  }
  a += b
  print(a.toHost())
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test_bool_param2{{.*}}
// CHECK: sil private @{{.*}}test_bool_param2{{.*}}
// CHECK: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>):
// CHECK-NEXT:   %2 = builtin "__tfop_Add__tt:t__"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK:  [[BOOLTENSOR:%.*]] = builtin "tensorflowReceive_0"<TensorHandle<Builtin.Int1>>() : $TensorHandle<Builtin.Int1>
// CHECK-NEXT:    [[BOOL:%.*]] = builtin "tf_tensor_to_i1"([[BOOLTENSOR]] : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
// CHECK-NEXT:    cond_br [[BOOL]]
// ...
// CHECK: }

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test_bool_param2{{.*}}
// CHECK: bb0(%0 : $Bool)
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorProgram
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN:%.*]](
// CHECK:  [[BOOLVAL:%.*]] = struct_extract %0 : $Bool, #Bool._value
// CHECK:  builtin "tensorflowSend_0"<Builtin.Int1>([[BOOLVAL]]
// CHECK: cond_br [[BOOLVAL]],

