// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify -Xllvm -tf-module-level-graph=false %s | %FileCheck %s

import TensorFlow

public func testTensor(a: Tensor<Float>, b: Tensor<Float>) {
  // expected-warning @-1 {{'a' implicitly copied to the accelerator}}
  var x = a
  x += x  // expected-note {{value used here}}

  x -= x  // expected-warning {{value implicitly copied to the host, use .toHost() to make transfer explicit}}

  _hostOp(x) // expected-note {{value used here}}
  var y = b.toAccelerator()
  y += y
  _hostOp(y)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testTensor{{.*}}
// CHECK:  sil private @{{.*}}testTensor{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : @unowned $TensorHandle<Float>, %1 : @unowned $TensorHandle<Float>):
// CHECK-NEXT:   [[A:%.*]] = graph_op "Add"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) {T$dtype: i32 1, __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Float>
// CHECK-NEXT:   {{.*}} = graph_op "Sub"([[A]] : $TensorHandle<Float>, [[A]] : $TensorHandle<Float>) {T$dtype: i32 1, __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Float>
// CHECK:        graph_op "tfc.SendToHost
// CHECK:        [[RESULT:%.*]] = graph_op "Add"(%1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>
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


public func testScalar(f: Float) {
  var x = Tensor<Float>(f) + Tensor<Float>(1.0)
  x += x
  _hostOp(x)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testScalar{{.*}}
// CHECK: sil private @{{.*}}testScalar{{.*}} : $@callee_owned (TensorHandle<Builtin.FPIEEE32>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : @unowned $TensorHandle<Builtin.FPIEEE32>):
// CHECK-NEXT:   %1 = unchecked_ref_cast %0 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK:        [[CONST:%.*]] = graph_op "Const"() {dtype$dtype: i32 1, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Float>
// CHECK:        [[ADD1:%.*]] = graph_op "Add"(%1 : $TensorHandle<Float>, [[CONST]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// CHECK:        [[ADD2:%.*]] = graph_op "Add"([[ADD1]] : $TensorHandle<Float>, [[ADD1:%.*]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
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

public func testExitBranch1(i: Int) {
  var x = Tensor<Float>(1.0)

  if i == 0 {
    fatalError()   // Should terminate the tensor program.
  }

  x += x
  _hostOp(x)
}

// The tensor program should have no branch.

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testExitBranch1{{.*}}
// CHECK: sil private @{{.*}}testExitBranch1{{.*}} : $@callee_owned () -> TensorHandle<Float> {
// CHECK: bb0:
// CHECK-NEXT:   [[CONST:%.*]] = graph_op "Const"() {dtype$dtype: i32 1, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Float>
// CHECK:        [[RET:%.*]] = graph_op "Add"([[CONST]] : $TensorHandle<Float>, [[CONST]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
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



public func testExitBranch2(i: Int) {
  var x = Tensor<Float>(1.0)

  if i == 0 {
    return
  }

  x += x    // expected-warning {{value implicitly copied to the host}}
  _hostOp(x)  // expected-note {{value used here}}
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testExitBranch2{{.*}}
// CHECK: sil private @{{.*}}testExitBranch2{{.*}} : $@callee_owned (TensorHandle<Builtin.Int1>) -> () {
// CHECK: bb0(%0 : @unowned $TensorHandle<Builtin.Int1>):
// CHECK:  graph_op "Const"()
// CHECK:  cond_br {{.*}}, bb2, bb1

// CHECK:      bb1:
// CHECK:        graph_op "Add"(
// CHECK:        graph_op "tfc.SendToHost
// CHECK-NEXT:   br bb2

// CHECK: bb2:
// CHECK-NEXT: tuple ()
// CHECK-NEXT:  return
// }


// This program results in a boolean parameter being passed in.
public func test_bool_param(cond: Bool, x: Tensor<Float>, y: Tensor<Float>) {
  var a = x.toAccelerator()
  let b = y.toAccelerator()

  if cond {
    a -= b
  }
  a += b
  _hostOp(a.toHost())
}


// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test_bool_param{{.*}}
// CHECK: sil private @{{.*}}test_bool_param{{.*}} : $@callee_owned (TensorHandle<Builtin.Int1>, TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float>
// CHECK: bb0(%0 : @unowned $TensorHandle<Builtin.Int1>, %1 : @unowned $TensorHandle<Float>, %2 : @unowned $TensorHandle<Float>):
// CHECK: %3 = graph_op "tf_tensor_to_i1"(%0 : $TensorHandle<Builtin.Int1>) {{.*}} : $Builtin.Int1
// CHECK: cond_br %3, bb1, bb2


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

public func test_bool_param2(cond: Bool, x: Tensor<Float>, y: Tensor<Float>) {
  var a = x.toAccelerator()
  let b = y.toAccelerator()

  a += b

  if cond {
    a -= b
  }
  a += b
  _hostOp(a.toHost())
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test_bool_param2{{.*}}
// CHECK: sil private @{{.*}}test_bool_param2{{.*}}
// CHECK: bb0(%0 : @unowned $TensorHandle<Float>, %1 : @unowned $TensorHandle<Float>, %2 : @unowned $TensorHandle<Builtin.Int1>):
// CHECK:         graph_op "Add"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// CHECK-NEXT:    [[BOOL:%.*]] = graph_op "tf_tensor_to_i1"(%2 : $TensorHandle<Builtin.Int1>) {{.*}} : $Builtin.Int1
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


public func test_multiple_ifs(status: Bool) {
  var a = Tensor<Int32>(0)
  let b = a
  if status {
    a += b
  }
  a += b
  if a.scalarized() > 10 {
    a += b
  }
  a -= b
  _hostOp(a)
}

// The results are different here because compiler optimizes well
// in the presence of graph_ops.
// CHECK-LABEL: --- XLA CFG Canonicalize: {{.*}}test_multiple_ifs{{.*}}
// CHECK-NEXT: [sequence
// CHECK-NEXT:   {condition Header: bb0
// CHECK-NEXT:     block bb1
// CHECK-NEXT:     block bb2}
// CHECK-NEXT:   {condition Header: bb3
// CHECK-NEXT:     block bb4
// CHECK-NEXT:     block bb5}
// CHECK-NEXT:   block bb6]

public func test_while1(maxCount: Int, arg1: Tensor<Float>, arg2: Tensor<Float>) {
  var a = arg1.toAccelerator()
  let b = arg2.toAccelerator()

  a += b

  var count = 0
  while count < maxCount {
    a -= b
    count += 1
  }
  a += b
  _hostOp(a.toHost())
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}test_while1{{.*}}
// CHECK: sil private @{{.*}}test_while1{{.*}}
// CHECK: bb0(%0 : @unowned $TensorHandle<Float>, %1 : @unowned $TensorHandle<Float>, %2 : @unowned $TensorHandle<Builtin.Int1>, %3 : @unowned $TensorHandle<Builtin.Int64>):
// CHECK-NEXT: graph_op "Const"() {dtype$dtype: i32 9, value$tensor: i64 0
// CHECK:      graph_op "Add"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>
// CHECK-NEXT: graph_op "tf_tensor_to_i1"(
// CHECK-NEXT: cond_br {{.*}}, bb2, bb1

// CHECK: bb3([[A:%.*]] : @trivial $TensorHandle<Float>, [[COUNT:%.*]] : @trivial $TensorHandle<Builtin.Int64>):
// CHECK:       [[NEXTA:%.*]] = graph_op "Sub"([[A]] : $TensorHandle<Float>, %1 : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// CHECK:       [[NEXTCOUNT:%.*]] = graph_op "Add"([[COUNT]] : $TensorHandle<Builtin.Int64>
// CHECK:       [[CONDT:%.*]] = graph_op "Less"([[NEXTCOUNT]] : $TensorHandle<Builtin.Int64>
// CHECK-NEXT:   [[COND:%.*]] = graph_op "tf_tensor_to_i1"([[CONDT]] : $TensorHandle<Builtin.Int1>) {{.*}} : $Builtin.Int1
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


// This should turn into a single tensor program with no sends to the
// accelerator.  Until we get shape inference though, we won't be able to
// disprove away the optional check, so we'll need to send a bit back to the
// host.
public func scalar_manipulation(a : Float) -> Tensor<Float> {
  let x = Tensor<Float>(a) + Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}}
  let y = x.scalar! + 2.0 // expected-note {{value used here}}
  let z = Tensor<Float>(y)
  return z+z
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}scalar_manipulation{{.*}}
// CHECK: sil private @{{.*}}scalar_manipulation{{.*}} : $@callee_owned (TensorHandle<Builtin.FPIEEE32>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : @unowned $TensorHandle<Builtin.FPIEEE32>):
// CHECK-NEXT:  %1 = unchecked_ref_cast %0 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK-NEXT:  [[CONST:%.*]] = graph_op "Const"() {dtype$dtype: i32 1, value$tensor: f32 0x3F800000 /* 1 */, __device: "ALL_DEVICES"} : $TensorHandle<Float>
// CHECK:       graph_op "Add"(%1 : $TensorHandle<Float>, [[CONST]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// CHECK:       graph_op "tfc.SendToHost
// CHECK-NEXT:  graph_op "Const"()
// CHECK:       graph_op "tfc.RecvFromHost
// CHECK:       graph_op "Add"
// CHECK-NEXT:  unchecked_ref_cast {{.*}} : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK:       graph_op "Add"(
// CHECK-NEXT:  return
// CHECK-NEXT:}


public func testCast(x: Tensor<Float>) -> Tensor<Int32> {
  // expected-warning @-1 {{'x' implicitly copied to the accelerator}}
  return Tensor<Int32>(x+x)  // expected-note {{value used here}}
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testCast
// CHECK: sil private @{{.*}}testCast{{.*}} : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Int32> {
// CHECK: bb0(%0 : @unowned $TensorHandle<Float>):
// CHECK:   [[ADD:%.*]] = graph_op "Add"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// CHECK:   [[CAST:%.*]] = graph_op "Cast"([[ADD]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Int32>
// CHECK:   return [[CAST]] : $TensorHandle<Int32>

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
 CHECK: bb0(%0 : @unowned $TensorHandle<Float>, %1 : @unowned $TensorHandle<Float>):
 CHECK:  [[PACK1:%.*]] = graph_op "Pack"([%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>]) {{.*}} : $TensorHandle<Float>
 CHECK:  [[PACK2:%.*]] = graph_op "Pack"([%1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>]) {{.*}} : $TensorHandle<Float>
 CHECK:  [[RET:%.*]] = graph_op "Add"([[PACK1]] : $TensorHandle<Float>, [[PACK2]] : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
 CHECK:  return [[RET]] : $TensorHandle<Float>
 CHECK: }
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
CHECK: bb0(%0 : @unowned $TensorHandle<Float>, %1 : @unowned $TensorHandle<Float>, %2 : @unowned $TensorHandle<Float>):
CHECK: return {{.*}} : $TensorHandle<Float>
CHECK: }
*/

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}liveOutTest{{.*}}
// CHECK: @{{.*}}liveOutTest{{.*}} : $@convention(thin) (@guaranteed Tensor<Float>, @guaranteed Tensor<Float>, @guaranteed Tensor<Float>) -> @owned Tensor<Float> {
// CHECK: [[RESULTBUF:%.*]] = alloc_stack $OpaquePointer
// CHECK: [[RESULTACCESS:%.*]] = begin_access [modify] [static] [[RESULTBUF]] : $*OpaquePointer
// CHECK: [[RESULTPTR:%.*]] = address_to_pointer [[RESULTACCESS]] : $*OpaquePointer to $Builtin.RawPointer
// CHECK: [[RESULTMP:%.*]] = struct $UnsafeMutablePointer<OpaquePointer> ([[RESULTPTR]] : $Builtin.RawPointer)
// CHECK: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation : $@convention(thin) (@guaranteed _TensorComputation, UnsafeMutablePointer<OpaquePointer>, Int) -> ()
// CHECK: {{.*}} = apply [[FINISHFN]]({{.*}}, [[RESULTMP]], {{.*}}) : $@convention(thin) (@guaranteed _TensorComputation, UnsafeMutablePointer<OpaquePointer>, Int) -> ()



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
    #tfop("TensorDataSet", values,
          Toutput_types$dtype: [Float.tensorFlowDataType],
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
          output_types$dtype: [Float.tensorFlowDataType], output_shapes: [TensorShape(1)])

  // REGISTER_OP("MakeIterator")
  //     .Input("dataset: variant")
  //     .Input("iterator: resource")
  //     .SetShapeFn(shape_inference::NoOutputs);
  () = #tfop("MakeIterator", dataset, iterator)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testResourceAndVariantsyyF
// CHECK:  [[VALUES:%.*]] = graph_op "Const"() {dtype$dtype: i32 1, value$tensor: [$Float: (f32 0x3F800000 /* 1 */), (f32 0x40000000 /* 2 */),
// CHECK:  [[DATASET:%.*]] = graph_op "TensorDataSet"([[VALUES]] : $TensorHandle<Float>
// CHECK:  [[ITERATOR:%.*]] = graph_op "Iterator"()
// CHECK:  graph_op "MakeIterator"([[DATASET]] : $VariantHandle, [[ITERATOR]] : $ResourceHandle) {{.*}}
// CHECK-LABEL: ----


public func testStringHandle() {
  let str: TensorHandle<String> = #tfop(
    "Const", dtype$dtype: String.tensorFlowDataType, value$tensor: "foo"
  )
  let _: TensorHandle<String> = #tfop(
    "Substr", str, Tensor<Int32>(0), Tensor<Int32>(1)
  )
  let _: TensorHandle<String> = #tfop(
    "Const", dtype$dtype: String.tensorFlowDataType,
    value$tensor: ["foo", "bar"],
    shape$shape: TensorShape(2)
  )
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testStringHandle
// CHECK: [[STR:%.*]] = graph_op "Const"() {dtype$dtype: i32 7, value$tensor: "foo", __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<String>
// CHECK: [[POS:%.*]] = graph_op "Const"() {dtype$dtype: i32 3, value$tensor: i32 0, __device: "ALL_DEVICES"} : $TensorHandle<Int32>
// CHECK: [[LEN:%.*]] = graph_op "Const"() {dtype$dtype: i32 3, value$tensor: i32 1, __device: "ALL_DEVICES"} : $TensorHandle<Int32>
// CHECK: graph_op "Substr"([[STR]] : $TensorHandle<String>, [[POS]] : $TensorHandle<Int32>, [[LEN]] : $TensorHandle<Int32>) {__device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<String>
// CHECK: graph_op "Const"() {dtype$dtype: i32 7, value$tensor: [$String: "foo", "bar"], shape$shape: [$Int32: (i32 2)], __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<String>


// b/76117368
// This function is explicit marked inline(never) so it shouldn't be inlined,
// even though it has tensor operands.
@inline(never)
func shouldntInline(_ a: Tensor<Float>) -> Tensor<Float> {
  let b = a.toAccelerator()
  return (b*b).toHost()
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}shouldntInline
// CHECK: bb0(%0 : @unowned $TensorHandle<Float>):
// CHECK:  [[RET:%.*]] = graph_op "Mul"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) {{.*}} : $TensorHandle<Float>
// CHECK:  return [[RET]] : $TensorHandle<Float>
// CHECK-LABEL: ----

public func testNotInlined() {
  let a = Tensor<Float>([1,2])+1
  _ = shouldntInline(a.toHost())
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testNotInlined
// CHECK: = graph_op "Const"()
// CHECK: [[RESULT:%.*]] = graph_op "Add"
// CHECK: return [[RESULT]]
// CHECK-LABEL: ---

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testNotInlined
// CHECK: [[FN:%.*]] = function_ref @{{.*}}shouldntInline
// CHECK: = apply [[FN]](


// b/76362738 - eliminate workaround needed in @noinline mutating method
public struct NonInlineMethodExample {
  var a = Tensor<Float>(1.0)
  var b = Tensor<Float>(2.0)

  @inline(never)
  public mutating func mutatingMethod() {  // expected-warning {{'self' implicitly copied}}
    a += b   // expected-note {{value used here}}
    b += a
  }
}


// b/77158282
// This was a miscompilation caused by us deleting all retain/release instructions
// involving a value that got moved to the accelerator.  In fact, we need
// to retain these if there is a host use, because they may be retaining the value!
@inline(never)
func noInlineUser(_ x: Tensor<Float>) {
  _hostOp(x)
}

public func testNoInlineUser() {
    let x = Tensor<Float>(1)
    noInlineUser(x)
    noInlineUser(x)
}

// NOTE: the output for STRICT_DA does not have strong_retain or release.
// The output should be verified and updated once the bug is fixed.
//
// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testNoInlineUser
// CHECK: [[X:%.*]] = alloc_ref $TensorHandle<Float>
// CHECK: [[XS:%.*]] = struct $Tensor<Float> ([[X]] : $TensorHandle<Float>)
// CHECK: [[FN:%.*]] = function_ref @${{.*}}noInlineUser
// CHECK-NEXT: apply [[FN]]([[XS]])
// CHECK-NEXT: apply [[FN]]([[XS]])
// CHECK-LABEL: } // end sil function{{.*}}testNoInlineUser

// b/77437755
public func test77437755(_ hiddenSize: Float) {
  let stddev = 1.0 / hiddenSize
  let t1 = Tensor<Float>(shape: [5], scalars: [1.0, 2.0, 3.0, 4.0, 5.0])
  _ = t1 * stddev
}

// CHECK-LABEL: ---- INPUT FUNCTION {{.*}}test77437755{{.*}} ----------
// CHECK: [[STDDEV:%.*]] = builtin "fdiv_FPIEEE32"
// CHECK: [[STDDEVT:%.*]] = graph_op "tfc.scalarToTensor"([[STDDEV]] : $Builtin.FPIEEE32) {{.*}} : $TensorHandle<Float>
// CHECK:  graph_op "Mul"({{.*}} : $TensorHandle<Float>, [[STDDEVT]] : $TensorHandle<Float>) {{.*}}
// CHECK-LABEL: ---- END OF INPUT FUNCTION ----------


@TensorFlowGraph
public func graphFuncReturningOpaqueHandles() -> (ResourceHandle, ResourceHandle) {
  let iterator : ResourceHandle =
    #tfop("Iterator", shared_name: "foo", container: "bar",
    output_shapes: [TensorShape()], output_types$dtype: [Float.tensorFlowDataType])
  let iterator2 : ResourceHandle =
    #tfop("Iterator", shared_name: "foo", container: "bar",
    output_shapes: [TensorShape()], output_types$dtype: [Float.tensorFlowDataType])
  return (iterator, iterator2)
}
// CHECK-LABEL --- TFPartition Accelerator Result: {{.*}}graphFuncReturningOpaqueHandles{{.*}}
// CHECK: bb0:
// CHECK:  [[A:%.*]] = graph_op "Iterator"() {shared_name: "foo", container: "bar", output_shapes: [$TensorShape: ([$Int32: ])], output_types$dtype: [$TensorDataType: (((i32 1)))], __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $ResourceHandle 
// CHECK:  [[B:%.*]] = graph_op "Iterator"() {shared_name: "foo", container: "bar", output_shapes: [$TensorShape: ([$Int32: ])], output_types$dtype: [$TensorDataType: (((i32 1)))], __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $ResourceHandle 
// CHECK:  [[C:%.*]] = tuple ([[A]] : $ResourceHandle, [[B]] : $ResourceHandle)
// CHECK:  return [[C]] : $(ResourceHandle, ResourceHandle)   

// Allow Any.Type as a type list member for empty type lists.
public func testSR8570() {
  () = #tfop("FooOp", Targuments$dtype: [] as [UInt32]) // expected-error {{op named 'FooOp' is not registered in TensorFlow}}
}

