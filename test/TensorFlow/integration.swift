// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify -Xllvm -tf-strict-deabstraction %s | %FileCheck %s -check-prefix=STRICTDA

import TensorFlow

public func testTensor(a: Tensor<Float>, b: Tensor<Float>) {
  // expected-warning @-1 {{'a' implicitly copied to the accelerator}}
  var x = a
  x += x  // expected-note {{value used here}}

  x -= x  // expected-warning {{value implicitly copied to the host, use .toHost() to make transfer explicit}}

  print(x) // expected-note {{value used here}}
  var y = b.toAccelerator()
  y += y
  print(y)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testTensor{{.*}}
// CHECK:  sil private @{{.*}}testTensor{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>):
// CHECK-NEXT:   %2 = metatype $@thick Float.Type
// CHECK-NEXT:   %3 = string_literal utf8 "/device:CPU:0"
// CHECK-NEXT:   %4 = builtin "__tfop_Add,$in,$in,T,__device"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, %2 : $@thick Float.Type, %3 : $Builtin.RawPointer) : $TensorHandle<Float>
// CHECK-NEXT:   %5 = metatype $@thick Float.Type
// CHECK-NEXT:   %6 = string_literal utf8 "/device:CPU:0"
// CHECK-NEXT:   %7 = builtin "__tfop_Sub,$in,$in,T,__device"(%4 : $TensorHandle<Float>, %4 : $TensorHandle<Float>, %5 : $@thick Float.Type, %6 : $Builtin.RawPointer) : $TensorHandle<Float>
// CHECK:        graph_op "tfc.SendToHost
// CHECK-NEXT:   metatype $@thick Float.Type
// CHECK-NEXT:   string_literal utf8 "/device:CPU:0"
// CHECK:        [[RESULT:%.*]] = builtin "__tfop_Add,$in,$in,T,__device"(%1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>
// CHECK-NEXT:   return [[RESULT]] : $TensorHandle<Float>


// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testTensor{{.*}}
// CHECK: @{{.*}}testTensor{{.*}} : $@convention(thin) (@guaranteed Tensor<Float>, @guaranteed Tensor<Float>) -> () {

// Graph lowering should succeed, producing a serialized program of 10+ bytes.
// CHECK: string_literal bytes
// CHECK-NEXT:  integer_literal $Builtin.Int64, {{[1-9][0-9]+}}
// CHECK-NOT: = apply

// We're passing two TensorHandle's in.
// CHECK: [[ALLOC:%.*]] = alloc_stack $(OpaquePointer, OpaquePointer)
// CHECK: ref_element_addr
// CHECK: begin_access [read] [static] [[ALLOC]] : $*(OpaquePointer, OpaquePointer)
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN:%.*]](
// CHECK: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation
// CHECK-NEXT: apply [[FINISHFN]]([[PROGRAM]],

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}testTensor{{.*}}
// STRICTDA:  sil private @{{.*}}testTensor{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float> {
// STRICTDA: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>):
// STRICTDA-NEXT:   [[A:%.*]] = graph_op "Add,i,i"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>)
// STRICTDA-NEXT:   {{.*}} = graph_op "Sub,i,i"([[A]] : $TensorHandle<Float>, [[A]] : $TensorHandle<Float>)
// STRICTDA:        graph_op "tfc.SendToHost
// STRICTDA:        [[RESULT:%.*]] = graph_op "Add,i,i"(%1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>
// STRICTDA-NEXT:   return [[RESULT]] : $TensorHandle<Float>


// STRICTDA-LABEL: --- TFPartition Host Result: {{.*}}testTensor{{.*}}
// STRICTDA: @{{.*}}testTensor{{.*}} : $@convention(thin) (@guaranteed Tensor<Float>, @guaranteed Tensor<Float>) -> () {

// Graph lowering should succeed, producing a serialized program of 10+ bytes.
// STRICTDA: string_literal bytes
// STRICTDA-NEXT:  integer_literal $Builtin.Int64, {{[1-9][0-9]+}}
// STRICTDA-NOT: = apply

// We're passing two TensorHandle's in.
// STRICTDA: [[ALLOC:%.*]] = alloc_stack $(OpaquePointer, OpaquePointer)
// STRICTDA: ref_element_addr
// STRICTDA: begin_access [read] [static] [[ALLOC]] : $*(OpaquePointer, OpaquePointer)
// STRICTDA: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// STRICTDA-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN:%.*]](
// STRICTDA: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation
// STRICTDA-NEXT: apply [[FINISHFN]]([[PROGRAM]],


public func testScalar(f: Float) { // expected-warning {{'f' implicitly copied to the accelerator}}
  var x = Tensor<Float>(f) // expected-note {{value used here}}
          +
          Tensor<Float>(1.0)
  x += x
  print(x)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testScalar{{.*}}
// CHECK: sil private @{{.*}}testScalar{{.*}} : $@callee_owned (TensorHandle<Builtin.FPIEEE32>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Builtin.FPIEEE32>):
// CHECK-NEXT:   %1 = unchecked_ref_cast %0 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK:        [[CONST:%.*]] = graph_op "Const"() {dtype$dtype: $Builtin.FPIEEE32, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:   [[CAST:%.*]] = unchecked_ref_cast [[CONST]] : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK:        [[ADD1:%.*]] = builtin "__tfop_Add,$in,$in,T,__device"(%1 : $TensorHandle<Float>, [[CAST]] : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
// CHECK:        [[ADD2:%.*]] = builtin "__tfop_Add,$in,$in,T,__device"([[ADD1]] : $TensorHandle<Float>, [[ADD1:%.*]] : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
// CHECK-NEXT:   return [[ADD2]] : $TensorHandle<Float>
// CHECK-NEXT: }


// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testScalar{{.*}}
// CHECK: sil @{{.*}}testScalar{{.*}} : $@convention(thin) (Float) -> () {

// Graph lowering succeeds on this function
// CHECK: string_literal bytes "{{.....}}
// CHECK-NEXT:  integer_literal $Builtin.Int64, {{[1-9]}}

// StartTensorComputation is called with one input tensor
// CHECK: [[TENSORS:%.*]] = struct $UnsafePointer<OpaquePointer> ({{%.*}} : $Builtin.RawPointer)
// CHECK-NEXT: [[TENSOR_COUNT:%.*]] = integer_literal $Builtin.Int64, 1
// CHECK-NEXT: [[TENSOR_COUNT_STRUCT:%.*]] = struct $Int ([[TENSOR_COUNT]] : $Builtin.Int64)
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN]]({{%.*}}, {{%.*}}, [[TENSORS]], [[TENSOR_COUNT_STRUCT]]
// CHECK: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation
// CHECK-NEXT: apply [[FINISHFN]]([[PROGRAM]],

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}testScalar{{.*}}
// STRICTDA: sil private @{{.*}}testScalar{{.*}} : $@callee_owned (TensorHandle<Builtin.FPIEEE32>) -> TensorHandle<Float> {
// STRICTDA: bb0(%0 : $TensorHandle<Builtin.FPIEEE32>):
// STRICTDA-NEXT:   %1 = unchecked_ref_cast %0 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// STRICTDA:        [[CONST:%.*]] = graph_op "Const"() {dtype$dtype: $Builtin.FPIEEE32, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Builtin.FPIEEE32>
// STRICTDA-NEXT:   [[CAST:%.*]] = unchecked_ref_cast [[CONST]] : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// STRICTDA:        [[ADD1:%.*]] = graph_op "Add,i,i"(%1 : $TensorHandle<Float>, [[CAST]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// STRICTDA:        [[ADD2:%.*]] = graph_op "Add,i,i"([[ADD1]] : $TensorHandle<Float>, [[ADD1:%.*]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// STRICTDA-NEXT:   return [[ADD2]] : $TensorHandle<Float>
// STRICTDA-NEXT: }


// STRICTDA-LABEL: --- TFPartition Host Result: {{.*}}testScalar{{.*}}
// STRICTDA: sil @{{.*}}testScalar{{.*}} : $@convention(thin) (Float) -> () {

// Graph lowering succeeds on this function
// STRICTDA: string_literal bytes "{{.....}}
// STRICTDA-NEXT:  integer_literal $Builtin.Int64, {{[1-9]}}

// StartTensorComputation is called with one input tensor
// STRICTDA: [[TENSORS:%.*]] = struct $UnsafePointer<OpaquePointer> ({{%.*}} : $Builtin.RawPointer)
// STRICTDA-NEXT: [[TENSOR_COUNT:%.*]] = integer_literal $Builtin.Int64, 1
// STRICTDA-NEXT: [[TENSOR_COUNT_STRUCT:%.*]] = struct $Int ([[TENSOR_COUNT]] : $Builtin.Int64)
// STRICTDA: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// STRICTDA-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN]]({{%.*}}, {{%.*}}, [[TENSORS]], [[TENSOR_COUNT_STRUCT]]
// STRICTDA: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation
// STRICTDA-NEXT: apply [[FINISHFN]]([[PROGRAM]],

public func testExitBranch1(i: Int) {
  var x = Tensor<Float>(1.0)

  if i == 0 {
    fatalError()   // Should terminate the tensor program.
  }

  x += x
  print(x)
}

// The tensor program should have no branch.

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testExitBranch1{{.*}}
// CHECK: sil private @{{.*}}testExitBranch1{{.*}} : $@callee_owned () -> TensorHandle<Float> {
// CHECK: bb0:
// CHECK-NEXT:   [[CONST:%.*]] = graph_op "Const"() {dtype$dtype: $Builtin.FPIEEE32, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:   [[TH:%.*]] = unchecked_ref_cast [[CONST]] : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK:        [[RET:%.*]] = builtin "__tfop_Add,$in,$in,T,__device"([[TH]] : $TensorHandle<Float>, [[TH]] : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
// CHECK-NEXT:   return [[RET]] : $TensorHandle<Float>
// CHECK-NEXT: }


// The host program should kill the tensor program if the early exit happens,
// and finish it on the normal path.

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testExitBranch1{{.*}}
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN]](
// CHECK: cond_br

// CHECK: bb1:
// CHECK: [[TERMFN:%.*]] = function_ref @_swift_tfc_TerminateTensorComputation
// CHECK-NEXT: apply [[TERMFN]]([[PROGRAM]]) : $@convention(thin) (@guaranteed _TensorComputation) -> ()
// CHECK: unreachable

// CHECK: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation
// CHECK-NEXT: apply [[FINISHFN]]([[PROGRAM]],


// The tensor program should have no branch.

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}testExitBranch1{{.*}}
// STRICTDA: sil private @{{.*}}testExitBranch1{{.*}} : $@callee_owned () -> TensorHandle<Float> {
// STRICTDA: bb0:
// STRICTDA-NEXT:   [[CONST:%.*]] = graph_op "Const"() {dtype$dtype: $Builtin.FPIEEE32, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Builtin.FPIEEE32>
// STRICTDA-NEXT:   [[TH:%.*]] = unchecked_ref_cast [[CONST]] : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// STRICTDA:        [[RET:%.*]] = graph_op "Add,i,i"([[TH]] : $TensorHandle<Float>, [[TH]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// STRICTDA-NEXT:   return [[RET]] : $TensorHandle<Float>
// STRICTDA-NEXT: }

// The host program should kill the tensor program if the early exit happens,
// and finish it on the normal path.

// STRICTDA-LABEL: --- TFPartition Host Result: {{.*}}testExitBranch1{{.*}}
// STRICTDA: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// STRICTDA-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN]](
// STRICTDA: cond_br

// STRICTDA: bb1:
// STRICTDA: [[TERMFN:%.*]] = function_ref @_swift_tfc_TerminateTensorComputation
// STRICTDA-NEXT: apply [[TERMFN]]([[PROGRAM]]) : $@convention(thin) (@guaranteed _TensorComputation) -> ()
// STRICTDA: unreachable

// STRICTDA: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation
// STRICTDA-NEXT: apply [[FINISHFN]]([[PROGRAM]],



public func testExitBranch2(i: Int) {
  var x = Tensor<Float>(1.0)

  // expected-warning @+1 {{implicitly copied to the accelerator}}
  if i == 0 {
    return
  }

  x += x    // expected-warning {{value implicitly copied to the host}}
  print(x)  // expected-note {{value used here}}
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testExitBranch2{{.*}}
// CHECK: sil private @{{.*}}testExitBranch2{{.*}} : $@callee_owned (TensorHandle<Builtin.Int1>) -> () {
// CHECK: bb0(%0 : $TensorHandle<Builtin.Int1>):
// CHECK:  graph_op "Const"()
// CHECK:  cond_br {{.*}}, bb2, bb1

// CHECK:      bb1:
// CHECK:        builtin "__tfop_Add,$in,$in,T,__device"(
// CHECK:        graph_op "tfc.SendToHost
// CHECK-NEXT:   br bb2

// CHECK: bb2:
// CHECK-NEXT: tuple ()
// CHECK-NEXT:  return
// }

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}testExitBranch2{{.*}}
// STRICTDA: sil private @{{.*}}testExitBranch2{{.*}} : $@callee_owned (TensorHandle<Builtin.Int1>) -> () {
// STRICTDA: bb0(%0 : $TensorHandle<Builtin.Int1>):
// STRICTDA:  graph_op "Const"()
// STRICTDA:  cond_br {{.*}}, bb2, bb1

// STRICTDA:      bb1:
// STRICTDA:        graph_op "Add,i,i"(
// STRICTDA:        graph_op "tfc.SendToHost
// STRICTDA-NEXT:   br bb2

// STRICTDA: bb2:
// STRICTDA-NEXT: tuple ()
// STRICTDA-NEXT:  return
// }


// This program results in a boolean parameter being passed in.
public func test_bool_param(cond: Bool, // expected-warning {{'cond' implicitly copied to the accelerator}}
                            x: Tensor<Float>, y: Tensor<Float>) {
  var a = x.toAccelerator()
  let b = y.toAccelerator()

  if cond {  // expected-note {{value used here}}
    a -= b
  }
  a += b
  print(a.toHost())
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test_bool_param{{.*}}
// CHECK: sil private @{{.*}}test_bool_param{{.*}} : $@callee_owned (TensorHandle<Builtin.Int1>, TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float>
// CHECK: bb0(%0 : $TensorHandle<Builtin.Int1>, %1 : $TensorHandle<Float>, %2 : $TensorHandle<Float>):
// CHECK: %3 = graph_op "tf_tensor_to_i1"(%0 : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
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

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}test_bool_param{{.*}}
// STRICTDA: sil private @{{.*}}test_bool_param{{.*}} : $@callee_owned (TensorHandle<Builtin.Int1>, TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float>
// STRICTDA: bb0(%0 : $TensorHandle<Builtin.Int1>, %1 : $TensorHandle<Float>, %2 : $TensorHandle<Float>):
// STRICTDA: %3 = graph_op "tf_tensor_to_i1"(%0 : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
// STRICTDA: cond_br %3, bb2, bb1


// STRICTDA-LABEL: --- TFPartition Host Result: {{.*}}test_bool_param{{.*}}
// STRICTDA: = function_ref @_swift_tfc_CreateCTensorHandle : $@convention(thin)
// STRICTDA-NEXT: = integer_literal $Builtin.Int32, 10
// STRICTDA-NEXT: = struct $UInt32 ({{.*}} : $Builtin.Int32)
// STRICTDA-NEXT:  = struct $TF_DataType ({{.*}} : $UInt32)
// STRICTDA-NEXT:  = alloc_stack $Builtin.Int1
// STRICTDA-NEXT: store
// STRICTDA-NEXT:  = begin_access [read] [static]
// STRICTDA-NEXT:  = apply {{.*}}<Builtin.Int1>({{.*}}, {{.*}}) : $@convention(thin)
// STRICTDA-NEXT:  end_access
// STRICTDA-NEXT:  dealloc_stack

public func test_bool_param2(cond: Bool, // expected-warning {{'cond' implicitly copied to the accelerator}}
                             x: Tensor<Float>, y: Tensor<Float>) {
  var a = x.toAccelerator()
  let b = y.toAccelerator()

  a += b

  if cond { // expected-note {{value used here}}
    a -= b
  }
  a += b
  print(a.toHost())
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test_bool_param2{{.*}}
// CHECK: sil private @{{.*}}test_bool_param2{{.*}}
// CHECK: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %2 : $TensorHandle<Builtin.Int1>):
// CHECK:         builtin "__tfop_Add,$in,$in,T,__device"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
// CHECK-NEXT:    [[BOOL:%.*]] = graph_op "tf_tensor_to_i1"(%2 : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
// CHECK-NEXT:    cond_br [[BOOL]]
// ...
// CHECK: }

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test_bool_param2{{.*}}
// CHECK: bb0(%0 : $Bool, %1 : $Tensor<Float>, %2 : $Tensor<Float>):
// CHECK: [[BOOLVAL:%.*]] = struct_extract %0 : $Bool, #Bool._value
// CHECK: function_ref @_swift_tfc_CreateCTensorHandle
// CHECK: [[BOOLADDR:%.*]] = alloc_stack $Builtin.Int1
// CHECK-NEXT: store [[BOOLVAL]] to [[BOOLADDR]] : $*Builtin.Int1
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN:%.*]](
// CHECK: cond_br [[BOOLVAL]],

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}test_bool_param2{{.*}}
// STRICTDA: sil private @{{.*}}test_bool_param2{{.*}}
// STRICTDA: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %2 : $TensorHandle<Builtin.Int1>):
// STRICTDA:         graph_op "Add,i,i"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// STRICTDA-NEXT:    [[BOOL:%.*]] = graph_op "tf_tensor_to_i1"(%2 : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
// STRICTDA-NEXT:    cond_br [[BOOL]]
// ...
// STRICTDA: }

// STRICTDA-LABEL: --- TFPartition Host Result: {{.*}}test_bool_param2{{.*}}
// STRICTDA: bb0(%0 : $Bool, %1 : $Tensor<Float>, %2 : $Tensor<Float>):
// STRICTDA: [[BOOLVAL:%.*]] = struct_extract %0 : $Bool, #Bool._value
// STRICTDA: function_ref @_swift_tfc_CreateCTensorHandle
// STRICTDA: [[BOOLADDR:%.*]] = alloc_stack $Builtin.Int1
// STRICTDA-NEXT: store [[BOOLVAL]] to [[BOOLADDR]] : $*Builtin.Int1
// STRICTDA: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// STRICTDA-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN:%.*]](
// STRICTDA: cond_br [[BOOLVAL]],


// expected-warning @+1 {{'status' implicitly copied to the accelerator}}
public func test_multiple_ifs(status: Bool) {
  var a = Tensor<Int32>(0)
  let b = a
  if status { // expected-note {{value used here}}
    a += b
  }
  a += b
  if status {
    a += b
  }
  a -= b
  _hostOp(a)
}

// CHECK-LABEL: --- XLA CFG Canonicalize: {{.*}}test_multiple_ifs{{.*}}
// CHECK-NEXT: [sequence
// CHECK-NEXT:   {condition Header: bb0
// CHECK-NEXT:     block bb2
// CHECK-NEXT:     block bb1}
// CHECK-NEXT:   {condition Header: bb3
// CHECK-NEXT:     block bb5
// CHECK-NEXT:     block bb4}
// CHECK-NEXT:   block bb6]

// The results are different here because compiler optimizes well
// in the presence of graph_ops.
// STRICTDA-LABEL: --- XLA CFG Canonicalize: {{.*}}test_multiple_ifs{{.*}}
// STRICTDA-NEXT: [sequence
// STRICTDA-NEXT:   {condition Header: bb0
// STRICTDA-NEXT:     block bb2
// STRICTDA-NEXT:     block bb1}
// STRICTDA-NEXT:   block bb3]

public func test_while1(maxCount: Int,  // expected-warning {{'maxCount' implicitly copied to the accelerator}}
                        arg1: Tensor<Float>, arg2: Tensor<Float>) {
  var a = arg1.toAccelerator()
  let b = arg2.toAccelerator()

  a += b

  var count = 0
  // expected-warning @+1 {{implicitly copied to the accelerator}}
  while count < maxCount { // expected-note {{value used here}}
    a -= b
    count += 1
  }
  a += b
  print(a.toHost())
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test_while1{{.*}}
// CHECK: sil private @{{.*}}test_while1{{.*}}
// CHECK: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %2 : $TensorHandle<Builtin.Int1>, %3 : $TensorHandle<Builtin.Int64>):
// CHECK-NEXT: graph_op "Const"() {dtype$dtype: $Builtin.Int64, value$tensor: i64 0
// CHECK:      builtin "__tfop_Add,$in,$in,T,__device"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>
// CHECK-NEXT: graph_op "tf_tensor_to_i1"(
// CHECK-NEXT: cond_br {{.*}}, bb2, bb1

// CHECK: bb3([[A:%.*]] : $TensorHandle<Float>, [[COUNT:%.*]] : $TensorHandle<Builtin.Int64>):
// CHECK:       [[NEXTA:%.*]] = builtin "__tfop_Sub,$in,$in,T,__device"([[A]] : $TensorHandle<Float>, %1 : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
// CHECK:       [[NEXTCOUNT:%.*]] = graph_op "Add,i,i"([[COUNT]] : $TensorHandle<Builtin.Int64>
// CHECK:       [[CONDT:%.*]] = graph_op "Less,i,i"([[NEXTCOUNT]] : $TensorHandle<Builtin.Int64>
// CHECK-NEXT:   [[COND:%.*]] = graph_op "tf_tensor_to_i1"([[CONDT]] : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
// CHECK-NEXT:   cond_br [[COND]], bb5, bb4

// CHECK: bb5:
// CHECK-NEXT: br bb3([[NEXTA]] : $TensorHandle<Float>, [[NEXTCOUNT]] : $TensorHandle<Builtin.Int64>)


// CHECK-LABEL: --- XLA CFG Canonicalize: {{.*}}test_while1{{.*}}
// CHECK-NEXT: [sequence
// CHECK-NEXT:   {condition Header: bb0
// CHECK-NEXT:     [sequence
// CHECK-NEXT:       <while Preheader: bb2, Header: bb3, exit: bb4
// CHECK-NEXT:         block bb5>
// CHECK-NEXT:       block bb4]
// CHECK-NEXT:     block bb1}
// CHECK-NEXT:   block bb6]

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}test_while1{{.*}}
// STRICTDA: sil private @{{.*}}test_while1{{.*}}
// STRICTDA: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %2 : $TensorHandle<Builtin.Int1>, %3 : $TensorHandle<Builtin.Int64>):
// STRICTDA-NEXT: graph_op "Const"() {dtype$dtype: $Builtin.Int64, value$tensor: i64 0
// STRICTDA:      graph_op "Add,i,i"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>
// STRICTDA-NEXT: graph_op "tf_tensor_to_i1"(
// STRICTDA-NEXT: cond_br {{.*}}, bb2, bb1

// STRICTDA: bb3([[A:%.*]] : $TensorHandle<Float>, [[COUNT:%.*]] : $TensorHandle<Builtin.Int64>):
// STRICTDA:       [[NEXTA:%.*]] = graph_op "Sub,i,i"([[A]] : $TensorHandle<Float>, %1 : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// STRICTDA:       [[NEXTCOUNT:%.*]] = graph_op "Add,i,i"([[COUNT]] : $TensorHandle<Builtin.Int64>
// STRICTDA:       [[CONDT:%.*]] = graph_op "Less,i,i"([[NEXTCOUNT]] : $TensorHandle<Builtin.Int64>
// STRICTDA-NEXT:   [[COND:%.*]] = graph_op "tf_tensor_to_i1"([[CONDT]] : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
// STRICTDA-NEXT:   cond_br [[COND]], bb5, bb4

// STRICTDA: bb5:
// STRICTDA-NEXT: br bb3([[NEXTA]] : $TensorHandle<Float>, [[NEXTCOUNT]] : $TensorHandle<Builtin.Int64>)


// STRICTDA-LABEL: --- XLA CFG Canonicalize: {{.*}}test_while1{{.*}}
// STRICTDA-NEXT: [sequence
// STRICTDA-NEXT:   {condition Header: bb0
// STRICTDA-NEXT:     [sequence
// STRICTDA-NEXT:       <while Preheader: bb2, Header: bb3, exit: bb4
// STRICTDA-NEXT:         block bb5>
// STRICTDA-NEXT:       block bb4]
// STRICTDA-NEXT:     block bb1}
// STRICTDA-NEXT:   block bb6]


// This should turn into a single tensor program with no sends to the
// accelerator.  Until we get shape inference though, we won't be able to
// disprove away the optional check, so we'll need to send a bit back to the
// host.
public func scalar_manipulation(a : Float) -> Tensor<Float> {
  // expected-warning @-1 {{'a' implicitly copied to the accelerator, use .toAccelerator() to make transfer explicit}}

  // expected-note @+1 {{value used here}}
  let x = Tensor<Float>(a) + Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}}
  let y = x.scalar! + 2.0 // expected-note {{value used here}}
  // expected-warning @-1 {{value implicitly copied to the accelerator}}

  let z = Tensor<Float>(y)
  return z+z
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}scalar_manipulation{{.*}}
// CHECK: sil private @{{.*}}scalar_manipulation{{.*}} : $@callee_owned (TensorHandle<Builtin.FPIEEE32>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Builtin.FPIEEE32>):
// CHECK-NEXT:  %1 = unchecked_ref_cast %0 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK-NEXT:  [[CONST:%.*]] = graph_op "Const"() {dtype$dtype: $Builtin.FPIEEE32, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:  [[CAST:%.*]] = unchecked_ref_cast [[CONST]] : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>

// CHECK:       builtin "__tfop_Add,$in,$in,T,__device"(%1 : $TensorHandle<Float>, [[CAST]] : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
// CHECK:       graph_op "tfc.SendToHost
// CHECK-NEXT:  graph_op "Const"()
// CHECK:       graph_op "tfc.RecvFromHost
// CHECK:       graph_op "Add,i,i"
// CHECK-NEXT:  unchecked_ref_cast {{.*}} : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK:       builtin "__tfop_Add,$in,$in,T,__device"(
// CHECK-NEXT:  return
// CHECK-NEXT:}

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}scalar_manipulation{{.*}}
// STRICTDA: sil private @{{.*}}scalar_manipulation{{.*}} : $@callee_owned (TensorHandle<Builtin.FPIEEE32>) -> TensorHandle<Float> {
// STRICTDA: bb0(%0 : $TensorHandle<Builtin.FPIEEE32>):
// STRICTDA-NEXT:  %1 = unchecked_ref_cast %0 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// STRICTDA-NEXT:  [[CONST:%.*]] = graph_op "Const"() {dtype$dtype: $Builtin.FPIEEE32, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Builtin.FPIEEE32>
// STRICTDA-NEXT:  [[CAST:%.*]] = unchecked_ref_cast [[CONST]] : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>

// STRICTDA:       graph_op "Add,i,i"(%1 : $TensorHandle<Float>, [[CAST]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// STRICTDA:       graph_op "tfc.SendToHost
// STRICTDA-NEXT:  graph_op "Const"()
// STRICTDA:       graph_op "tfc.RecvFromHost
// STRICTDA:       graph_op "Add,i,i"
// STRICTDA-NEXT:  unchecked_ref_cast {{.*}} : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// STRICTDA:       graph_op "Add,i,i"(
// STRICTDA-NEXT:  return
// STRICTDA-NEXT:}


public func testCast(x: Tensor<Float>) -> Tensor<Int32> {
  // expected-warning @-1 {{'x' implicitly copied to the accelerator}}
  return Tensor<Int32>(x+x)  // expected-note {{value used here}}
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testCast
// CHECK: sil private @{{.*}}testCast{{.*}} : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Int32> {
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK:   %3 = builtin "__tfop_Add,$in,$in,T,__device"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
// CHECK:   %5 = metatype $@thick Int32.Type
// CHECK:   %7 = builtin "__tfop_Cast,$in,SrcT,DstT,__device"(%3 : $TensorHandle<Float>, %4 : $@thick Float.Type, %5 : $@thick Int32.Type, {{.*}}) : $TensorHandle<Int32>
// CHECK:   return %7 : $TensorHandle<Int32>

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}testCast
// STRICTDA: sil private @{{.*}}testCast{{.*}} : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Int32> {
// STRICTDA: bb0(%0 : $TensorHandle<Float>):
// STRICTDA:   [[ADD:%.*]] = graph_op "Add,i,i"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// STRICTDA:   [[CAST:%.*]] = graph_op "Cast,i"([[ADD]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Int32>
// STRICTDA:   return [[CAST]] : $TensorHandle<Int32>

// expected-warning @+1 2 {{implicitly copied to the accelerator}}
public func testInputListArguments(a: TensorHandle<Float>, b: Tensor<Float>) -> Tensor<Float> {
  // Pack takes an input list, not multiple inputs.  Here we're checking that
  // we can pass in an array of Tensor's and an array of TensorHandle's.
  let x: TensorHandle<Float> = #tfop("Pack", [a, a, a])  // expected-note {{value used here}}
  let y: TensorHandle<Float> = #tfop("Pack", [b, b, b])  // expected-note {{value used here}}
  return (Tensor(handle: x)+Tensor(handle: y)).toHost()
}

/*
 CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testInputListArguments
 CHECK: sil private @{{.*}}testInputListArguments{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float> {
 CHECK: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>):
 CHECK:   %2 = metatype $@thin TensorHandle<Float>.Type
 CHECK:   [[PACK1:%.*]] = builtin "__tfop_Pack,$in,$inelt,$inelt,$inelt,__device"(%2 : $@thin TensorHandle<Float>.Type, %0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
 CHECK:  [[TYPE:%.*]] = metatype $@thin Tensor<Float>.Type
 CHECK:  [[PACK2:%.*]] = builtin "__tfop_Pack,$in,$inelt,$inelt,$inelt,__device"([[TYPE]] : $@thin Tensor<Float>.Type, %1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
 CHECK:  [[RET:%.*]] = builtin "__tfop_Add,$in,$in,T,__device"([[PACK1]] : $TensorHandle<Float>, [[PACK2]] : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
 CHECK:  return [[RET]] : $TensorHandle<Float>
 CHECK: }
*/

/*
 STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}testInputListArguments
 STRICTDA: sil private @{{.*}}testInputListArguments{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float> {
 STRICTDA: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>):
 STRICTDA:  [[PACK1:%.*]] = graph_op "Pack,L,e,e,e"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
 STRICTDA:  [[PACK2:%.*]] = graph_op "Pack,L,e,e,e"(%1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
 STRICTDA:  [[RET:%.*]] = graph_op "Add,i,i"([[PACK1]] : $TensorHandle<Float>, [[PACK2]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
 STRICTDA:  return [[RET]] : $TensorHandle<Float>
 STRICTDA: }
*/

// This should produce exactly one live out value in the call to
// _swift_tfc_FinishTensorComputation.
public func liveOutTest(
  a: Tensor<Float>, // expected-warning {{'a' implicitly copied to the accelerator, use .toAccelerator() to make transfer explicit}}
  b: Tensor<Float>, // expected-warning {{'b' implicitly copied to the accelerator, use .toAccelerator() to make transfer explicit}}
  c: Tensor<Float> // expected-warning {{'c' implicitly copied to the accelerator, use .toAccelerator() to make transfer explicit}}
) -> Tensor<Float> {
  return a+b+c // expected-note 3 {{value used here}}
}

/*
 CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}liveOutTest
 CHECK: sil private @{{.*}}liveOutTest{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float> {
 CHECK: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %2 : $TensorHandle<Float>):
 CHECK:  return {{.*}} : $TensorHandle<Float>
 CHECK-LABEL: }
 */

/*
 CHECK-LABEL: --- TFPartition Host Result: {{.*}}liveOutTest{{.*}}
 CHECK: @{{.*}}liveOutTest{{.*}} : $@convention(thin) (@guaranteed Tensor<Float>, @guaranteed Tensor<Float>, @guaranteed Tensor<Float>) -> @owned Tensor<Float> {

 // [[RESULTBUF:%.*]] = alloc_stack $OpaquePointer
 // [[RESULTACCESS:%.*]] = begin_access [modify] [static] [[RESULTBUF]] : $*OpaquePointer
 // [[RESULTPTR:%.*]] = address_to_pointer [[RESULTACCESS]] : $*OpaquePointer to $Builtin.RawPointer
 // [[RESULTMP:%.*]] = struct $UnsafeMutablePointer<OpaquePointer> ([[RESULTPTR]] : $Builtin.RawPointer)
 // [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation : $@convention(thin) (@owned _TensorComputation, UnsafeMutablePointer<OpaquePointer>, Int) -> ()
 // %53 = apply [[FINISHFN]]({{.*}}, [[RESULTMP]], {{.*}}) : $@convention(thin) (@owned _TensorComputation, UnsafeMutablePointer<OpaquePointer>, Int) -> ()
*/

/*
STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}liveOutTest
STRICTDA: sil private @{{.*}}liveOutTest{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float> {
STRICTDA: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %2 : $TensorHandle<Float>):
STRICTDA: return {{.*}} : $TensorHandle<Float>
STRICTDA: }
*/

// STRICTDA-LABEL: --- TFPartition Host Result: {{.*}}liveOutTest{{.*}}
// STRICTDA: @{{.*}}liveOutTest{{.*}} : $@convention(thin) (@guaranteed Tensor<Float>, @guaranteed Tensor<Float>, @guaranteed Tensor<Float>) -> @owned Tensor<Float> {
// STRICTDA: [[RESULTBUF:%.*]] = alloc_stack $OpaquePointer
// STRICTDA: [[RESULTACCESS:%.*]] = begin_access [modify] [static] [[RESULTBUF]] : $*OpaquePointer
// STRICTDA: [[RESULTPTR:%.*]] = address_to_pointer [[RESULTACCESS]] : $*OpaquePointer to $Builtin.RawPointer
// STRICTDA: [[RESULTMP:%.*]] = struct $UnsafeMutablePointer<OpaquePointer> ([[RESULTPTR]] : $Builtin.RawPointer)
// STRICTDA: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation : $@convention(thin) (@guaranteed _TensorComputation, UnsafeMutablePointer<OpaquePointer>, Int) -> ()
// STRICTDA: {{.*}} = apply [[FINISHFN]]({{.*}}, [[RESULTMP]], {{.*}}) : $@convention(thin) (@guaranteed _TensorComputation, UnsafeMutablePointer<OpaquePointer>, Int) -> ()



/// This tests some operations using resources and variants.
public func testResourceAndVariants() {
  let values = Tensor<Float>([1,2,3,4,5,6])

  // REGISTER_OP("TensorDataset")
  //     .Input("components: Toutput_types")
  //     .Output("handle: variant")
  //     .Attr("Toutput_types: list(type) >= 1")
  //     .Attr("output_shapes: list(shape) >= 1")
  let dataset: VariantHandle =
    // expected-error @+1 {{op named 'TensorDataSet' is not registered in TensorFlow}}
    #tfop("TensorDataSet", values, Toutput_types: [Float.self],
          output_shapes: [TensorShape(1)])

  // REGISTER_OP("Iterator")
  //     .Output("handle: resource")
  //     .Attr("shared_name: string")
  //     .Attr("container: string")
  //     .Attr("output_types: list(type) >= 1")
  //     .Attr("output_shapes: list(shape) >= 1")
  //     .SetShapeFn(shape_inference::ScalarShape);
  let iterator: ResourceHandle =
    #tfop("Iterator", shared_name: "foo", container: "bar",
          output_types: [Float.self], output_shapes: [TensorShape(1)])

  // REGISTER_OP("MakeIterator")
  //     .Input("dataset: variant")
  //     .Input("iterator: resource")
  //     .SetShapeFn(shape_inference::NoOutputs);
  () = #tfop("MakeIterator", dataset, iterator)
}

/*
CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testResourceAndVariantsyyF
CHECK:  [[values:%.*]] = graph_op "Const"() {dtype: $Float, value$tensor: [$Float: (f32 0x3F800000 /* 1 */), (f32 0x40000000 /* 2 */),
CHECK:  [[dataset:%.*]] = builtin "__tfop_TensorDataSet,$in,Toutput_types$array,$elt,output_shapes$shapearray,$shape,$elt,__device"([[values]] : $TensorHandle<Float>
CHECK:  [[iterator:%.*]] = builtin "__tfop_Iterator,shared_name,container,output_types$array,$elt,output_shapes$shapearray,$shape,$elt,__device"({{.*}} : $Builtin.RawPointer, {{.*}} : $Builtin.RawPointer
CHECK:  builtin "__tfop_MakeIterator,$in,$in,__device"([[dataset]] : $VariantHandle, [[iterator]] : $ResourceHandle, {{.*}}) : $()
CHECK-LABEL: ----
*/

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}testResourceAndVariantsyyF
// STRICTDA:  [[VALUES:%.*]] = graph_op "Const"() {dtype: $Float, value$tensor: [$Float: (f32 0x3F800000 /* 1 */), (f32 0x40000000 /* 2 */),
// STRICTDA:  [[DATASET:%.*]] = graph_op "TensorDataSet,i"([[VALUES]] : $TensorHandle<Float>
// STRICTDA:  [[ITERATOR:%.*]] = graph_op "Iterator"()
// STRICTDA:  graph_op "MakeIterator,i,i"([[DATASET]] : $VariantHandle, [[ITERATOR]] : $ResourceHandle) {{.*}} 
// STRICTDA-LABEL: ----


// b/76117368
// This function is explicit marked inline(never) so it shouldn't be inlined,
// even though it has tensor operands.
@inline(never)
func shouldntInline(_ a: Tensor<Float>) -> Tensor<Float> {
  let b = a.toAccelerator()
  return (b*b).toHost()
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}shouldntInline
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK:  %3 = builtin "__tfop_Mul,$in,$in,T,__device"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, {{.*}}) : $TensorHandle<Float>
// CHECK:  return %3 : $TensorHandle<Float>
// CHECK-LABEL: ----

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}shouldntInline
// STRICTDA: bb0(%0 : $TensorHandle<Float>):
// STRICTDA:  [[RET:%.*]] = graph_op "Mul,i,i"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// STRICTDA:  return [[RET]] : $TensorHandle<Float>
// STRICTDA-LABEL: ----

public func testNotInlined() {
  let a = Tensor<Float>([1,2])+1
  _ = shouldntInline(a.toHost())
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testNotInlined
// CHECK: = graph_op "Const"()
// CHECK: [[RESULT:%.*]] = builtin "__tfop_Add,
// CHECK: return [[RESULT]]
// CHECK-LABEL: ---

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testNotInlined
// CHECK: [[FN:%.*]] = function_ref @{{.*}}shouldntInline
// CHECK: = apply [[FN]](

// STRICTDA-LABEL: --- TFPartition Accelerator Result: {{.*}}testNotInlined
// STRICTDA: = graph_op "Const"()
// STRICTDA: [[RESULT:%.*]] = graph_op "Add,i,i"
// STRICTDA: return [[RESULT]]
// STRICTDA-LABEL: ---

// STRICTDA-LABEL: --- TFPartition Host Result: {{.*}}testNotInlined
// STRICTDA: [[FN:%.*]] = function_ref @{{.*}}shouldntInline
// STRICTDA: = apply [[FN]](


// b/76362738 - eliminate workaround needed in @noinline mutating method
public struct NonInlineMethodExample {
  var a = Tensor<Float>(1.0)
  var b = Tensor<Float>(2.0)

  @inline(never)
  public mutating func mutatingMethod() {  // expected-warning 2 {{value implicitly copied}}
    a += b   // expected-note 2 {{value used here}}
    b += a
  }
}


// b/77158282
// This was a miscompilation caused by us deleting all retain/release instructions
// involving a value that got moved to the accelerator.  In fact, we need
// to retain these if there is a host use, because they may be retaining the value!
@inline(never)
func noInlineUser(_ x: Tensor<Float>) {
  print(x)
}

public func testNoInlineUser() {
    let x = Tensor<Float>(1)
    noInlineUser(x)
    noInlineUser(x)
}

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testNoInlineUser
// CHECK: [[X:%.*]] = alloc_ref $TensorHandle<Float>
// CHECK: [[XS:%.*]] = struct $Tensor<Float> ([[X]] : $TensorHandle<Float>)
// CHECK:  strong_retain [[SOME_T:%.*]] : $TensorHandle<Float>
// CHECK-NEXT:  strong_release [[SOME_T]] : $TensorHandle<Float>
// CHECK:  [[FN:%.*]] = function_ref @${{.*}}noInlineUser
// CHECK-NEXT: apply [[FN]]([[XS]])
// CHECK-NEXT: apply [[FN]]([[XS]])
// CHECK-LABEL: } // end sil function{{.*}}testNoInlineUser

// NOTE: the output for STRICT_DA does not even have strong_retain or release.
// The output should be verified and updated once the bug is fixed.
//
// STRICTDA-LABEL: --- TFPartition Host Result: {{.*}}testNoInlineUser
// STRICTDA: [[X:%.*]] = alloc_ref $TensorHandle<Float>
// STRICTDA: [[XS:%.*]] = struct $Tensor<Float> ([[X]] : $TensorHandle<Float>)
// STRICTDA: [[FN:%.*]] = function_ref @${{.*}}noInlineUser
// STRICTDA-NEXT: apply [[FN]]([[XS]])
// STRICTDA-NEXT: apply [[FN]]([[XS]])
// STRICTDA-LABEL: } // end sil function{{.*}}testNoInlineUser

// b/77437755
public func test77437755(_ hiddenSize: Float) {
  let stddev = 1.0 / hiddenSize  // expected-warning {{method result implicitly copied to the accelerator}}
  let t1 = Tensor<Float>(shape: [5], scalars: [1.0, 2.0, 3.0, 4.0, 5.0])
  _ = t1 * stddev  // expected-note {{value used here}}
}

// CHECK-LABEL: ---- INPUT FUNCTION {{.*}}test77437755{{.*}} ----------
// CHECK: [[STDDEV:%.*]] = builtin "fdiv_FPIEEE32"
// CHECK: [[STDDEVT:%.*]] = builtin "__tfop_tfc.scalarToTensor,$in"([[STDDEV]] : $Builtin.FPIEEE32) : $TensorHandle<Float>
// CHECK:  builtin "__tfop_Mul,$in,$in,T"({{.*}} : $TensorHandle<Float>, [[STDDEVT]] : $TensorHandle<Float>, {{.*}})
// CHECK-LABEL: ---- END OF INPUT FUNCTION ----------


// STRICTDA-LABEL: ---- INPUT FUNCTION {{.*}}test77437755{{.*}} ----------
// STRICTDA: [[STDDEV:%.*]] = builtin "fdiv_FPIEEE32"
// STRICTDA: [[STDDEVT:%.*]] = graph_op "tfc.scalarToTensor,s"([[STDDEV]] : $Builtin.FPIEEE32) {{.*}} : $TensorHandle<Float>
// STRICTDA:  graph_op "Mul,i,i"({{.*}} : $TensorHandle<Float>, [[STDDEVT]] : $TensorHandle<Float>) {{.*}}
// STRICTDA-LABEL: ---- END OF INPUT FUNCTION ----------
