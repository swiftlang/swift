// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil %s -verify | %FileCheck %s
import TensorFlow

public func testTensor(a: Tensor<Float>, b: Tensor<Float>) {
  // expected-warning @-1 {{'a' implicitly copied to the accelerator}}
  var x = a
  x += x  // expected-note {{value used here}}

  x -= x  // expected-warning {{value implicitly copied to the host, use .toHost() to make transfer explicit}}
  // GraphGen doesn't support sends yet: expected-error @-1 {{internal error generating TensorFlow graph}}

  print(x) // expected-note {{value used here}}
  var y = b.toDevice()
  y += y
  print(y)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testTensor{{.*}}
// CHECK:  sil private @{{.*}}testTensor{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>):
// CHECK-NEXT:   %2 = builtin "__tfop_Add,tt:t"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %3 = builtin "__tfop_Sub,tt:t"(%2 : $TensorHandle<Float>, %2 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %4 = builtin "tensorflowSend_0"<TensorHandle<Float>>(%3 : $TensorHandle<Float>) : $()
// CHECK-NEXT:   %5 = builtin "__tfop_Add,tt:t"(%1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   return %5 : $TensorHandle<Float>


// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testTensor{{.*}}
// CHECK: sil shared @{{.*}}testTensor{{.*}} : $@convention(thin) (@guaranteed Tensor<Float>, @guaranteed Tensor<Float>) -> () {

// Graph lowering fails on testTensor because it requires send and receive instructions.
// CHECK: string_literal bytes ""
// CHECK-NEXT:  integer_literal $Builtin.Int64, 0
// CHECK-NOT: = apply

// We're passing one TensorHandle in.
// CHECK: [[ALLOC:%.*]] = alloc_stack $(OpaquePointer, OpaquePointer)
// CHECK: ref_element_addr
// CHECK: begin_access [read] [static] [[ALLOC]] : $*(OpaquePointer, OpaquePointer)
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN:%.*]](
// CHECK: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation
// CHECK-NEXT: apply [[FINISHFN]]([[PROGRAM]],

public func testScalar(f: Float) { // expected-warning {{'f' implicitly copied to the accelerator}}
  var x = Tensor<Float>(f) // expected-note {{value used here}}
          +
          Tensor<Float>(1.0)
  x += x
  print(x)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testScalar{{.*}}
// CHECK: sil private @{{.*}}testScalar{{.*}} : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK-NEXT:   %1 = integer_literal $Builtin.Int32, 1
// CHECK-NEXT:   %2 = float_literal $Builtin.FPIEEE32, 0x3F800000 // 1
// CHECK-NEXT:   %3 = builtin "__tfop_Const,:t,dtype$dtype,value$tensor"(%1 : $Builtin.Int32, %2 : $Builtin.FPIEEE32) : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:   %4 = builtin "__tfop_Add,tt:t"(%0 : $TensorHandle<Float>, %3 : $TensorHandle<Builtin.FPIEEE32>) : $TensorHandle<Float>
// CHECK-NEXT:   %5 = builtin "__tfop_Add,tt:t"(%4 : $TensorHandle<Float>, %4 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   return %5 : $TensorHandle<Float>
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
  print(x)
}

// The tensor program should have no branch.

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testExitBranch1{{.*}}
// CHECK: sil private @{{.*}}testExitBranch1{{.*}} : $@callee_owned () -> TensorHandle<Float> {
// CHECK: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int32, 1
// CHECK-NEXT:   %1 = float_literal $Builtin.FPIEEE32, 0x3F800000 // 1
// CHECK-NEXT:   %2 = builtin "__tfop_Const,:t,dtype$dtype,value$tensor"(%0 : $Builtin.Int32, %1 : $Builtin.FPIEEE32) : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:   %3 = builtin "__tfop_Add,tt:t"(%2 : $TensorHandle<Builtin.FPIEEE32>, %2 : $TensorHandle<Builtin.FPIEEE32>) : $TensorHandle<Float>
// CHECK-NEXT:   return %3 : $TensorHandle<Float>
// CHECK-NEXT: }


// The host program should kill the tensor program if the early exit happens,
// and finish it on the normal path.

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}testExitBranch1{{.*}}
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN]](
// CHECK: cond_br

// CHECK: bb1:
// CHECK: [[TERMFN:%.*]] = function_ref @_swift_tfc_TerminateTensorComputation
// CHECK-NEXT: apply [[TERMFN]]([[PROGRAM]]) : $@convention(thin) (@owned _TensorComputation) -> ()
// CHECK: unreachable

// CHECK: bb2:
// CHECK: [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation
// CHECK-NEXT: apply [[FINISHFN]]([[PROGRAM]],



public func testExitBranch2(i: Int) {
  var x = Tensor<Float>(1.0)

  // expected-warning @+1 {{implicitly copied to the accelerator}}
  if i == 0 { 
    return
  }

  // expected-error @+1 {{GraphGen cannot lower a 'send' to the host yet}}
  x += x    // expected-warning {{value implicitly copied to the host}}
  print(x)  // expected-note {{value used here}}
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testExitBranch2{{.*}}
// CHECK: sil private @{{.*}}testExitBranch2{{.*}} : $@callee_owned (TensorHandle<Builtin.Int1>) -> () {
// CHECK: bb0(%0 : $TensorHandle<Builtin.Int1>):
// CHECK:  builtin "__tfop_Const,:t
// CHECK:  cond_br {{.*}}, bb2, bb1

// CHECK:      bb1:
// CHECK-NEXT:   builtin "__tfop_Add,tt:t"(
// CHECK-NEXT:   builtin "tensorflowSend_0"<TensorHandle<Float>>(
// CHECK-NEXT:   br bb2

// CHECK: bb2:
// CHECK-NEXT: tuple ()
// CHECK-NEXT:  return
// }




// This program results in a boolean parameter being passed in.
public func test_bool_param(cond: Bool, // expected-warning {{'cond' implicitly copied to the accelerator}}
                            x: Tensor1D<Float>, y: Tensor1D<Float>) {
  var a = x.toDevice()
  let b = y.toDevice()

  if cond {  // expected-note {{value used here}}
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


public func test_bool_param2(cond: Bool, // expected-warning {{'cond' implicitly copied to the accelerator}}
                             x: Tensor1D<Float>, y: Tensor1D<Float>) {
  var a = x.toDevice()
  let b = y.toDevice()

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
// CHECK-NEXT:    builtin "__tfop_Add,tt:t"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:    [[BOOL:%.*]] = builtin "tf_tensor_to_i1"(%2 : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
// CHECK-NEXT:    cond_br [[BOOL]]
// ...
// CHECK: }

// CHECK-LABEL: --- TFPartition Host Result: {{.*}}test_bool_param2{{.*}}
// CHECK: bb0(%0 : $Bool, %1 : $Tensor1D<Float>, %2 : $Tensor1D<Float>):
// CHECK: [[BOOLVAL:%.*]] = struct_extract %0 : $Bool, #Bool._value
// CHECK: function_ref @_swift_tfc_CreateCTensorHandle
// CHECK: [[BOOLADDR:%.*]] = alloc_stack $Builtin.Int1
// CHECK-NEXT: store [[BOOLVAL]] to [[BOOLADDR]] : $*Builtin.Int1
// CHECK: [[STARTFN:%.*]] = function_ref @_swift_tfc_StartTensorComputation
// CHECK-NEXT: [[PROGRAM:%.*]] = apply [[STARTFN:%.*]](
// CHECK: cond_br [[BOOLVAL]],


public func test_while1(maxCount: Int,  // expected-warning {{'maxCount' implicitly copied to the accelerator}}
                        arg1: Tensor1D<Float>, arg2: Tensor1D<Float>) {
  var a = arg1.toDevice()
  let b = arg2.toDevice()

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
// CHECK-NEXT: integer_literal $Builtin.Int32, 9
// CHECK-NEXT: integer_literal $Builtin.Int64, 0
// CHECK-NEXT: builtin "__tfop_Const,:t,dtype$dtype,value$tensor"(
// CHECK-NEXT: builtin "__tfop_Add,tt:t"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>)
// CHECK-NEXT: builtin "tf_tensor_to_i1"(
// CHECK-NEXT: cond_br {{.*}}, bb2, bb1

// CHECK: bb3([[COUNT:%.*]] : $TensorHandle<Builtin.Int64>, [[A:%.*]] : $TensorHandle<Float>):
// CHECK-NEXT:  [[NEXTA:%.*]] = builtin "__tfop_Sub,tt:t"([[A:%.*]] : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:  [[NEXTCOUNT:%.*]] = builtin "__tfop_Add,tt:t"([[COUNT:%.*]] : $TensorHandle<Builtin.Int64>,
// CHECK-NEXT: [[CONDT:%.*]] = builtin "__tfop_Less,tt:t"([[NEXTCOUNT]] : $TensorHandle<Builtin.Int64>,
// CHECK-NEXT:   [[COND:%.*]] = builtin "tf_tensor_to_i1"([[CONDT]] : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
// CHECK-NEXT:   cond_br [[COND]], bb5, bb4

// CHECK: bb5:
// CHECK-NEXT: br bb3([[NEXTCOUNT]] : $TensorHandle<Builtin.Int64>, [[NEXTA]] : $TensorHandle<Float>)


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
  // expected-warning @-1 {{'a' implicitly copied to the accelerator, use .toDevice() to make transfer explicit}}

  // expected-note @+1 {{value used here}}
  let x = Tensor<Float>(a) + Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}} expected-error {{GraphGen cannot lower a 'send' to the host yet}}
  let y = x.scalar! + 2.0    // expected-note {{value used here}}
  // expected-warning @-1 {{value implicitly copied to the accelerator}}

  let z = Tensor<Float>(y)
  return z+z
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}scalar_manipulation{{.*}}
// CHECK: sil private @{{.*}}scalar_manipulation{{.*}} : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK-NEXT:  %1 = integer_literal $Builtin.Int32, 1
// CHECK-NEXT:  %2 = float_literal $Builtin.FPIEEE32, 0x3F800000 // 1
// CHECK-NEXT:  %3 = builtin "__tfop_Const,:t,dtype$dtype,value$tensor"(%1 : $Builtin.Int32, %2 : $Builtin.FPIEEE32) : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:  %4 = builtin "__tfop_Add,tt:t"(%0 : $TensorHandle<Float>, %3 : $TensorHandle<Builtin.FPIEEE32>) : $TensorHandle<Float>
// CHECK-NEXT:  %5 = builtin "tensorflowSend_1"<TensorHandle<Float>>(%4 : $TensorHandle<Float>) : $()
// CHECK-NEXT:  %6 = integer_literal $Builtin.Int32, 1
// CHECK-NEXT:  %7 = float_literal $Builtin.FPIEEE32, 0x40000000 // 2
// CHECK-NEXT:  %8 = builtin "__tfop_Const,:t,dtype$dtype,value$tensor"(%6 : $Builtin.Int32, %7 : $Builtin.FPIEEE32) : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:  %9 = builtin "tensorflowReceive_0"<TensorHandle<Builtin.FPIEEE32>>() : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:  %10 = builtin "__tfop_Add,tt:t"(%9 : $TensorHandle<Builtin.FPIEEE32>, %8 : $TensorHandle<Builtin.FPIEEE32>) : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:  %11 = builtin "__tfop_Add,tt:t"(%10 : $TensorHandle<Builtin.FPIEEE32>, %10 : $TensorHandle<Builtin.FPIEEE32>) : $TensorHandle<Float>
// CHECK-NEXT:  return %11 : $TensorHandle<Float>
// CHECK-NEXT:}


public func testSelect(conds1: Tensor<Bool>, x1: Tensor<Float>, y1: Tensor<Float>)
  -> Tensor<Float> {
  let conds = conds1.toDevice()
  let x = x1.toDevice()
  let y = y1.toDevice()

  let result = conds.selecting(x+x, y)*y

  return result.toHost()
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testSelect
// CHECK: sil private @{{.*}}testSelect{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Bool>, TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Bool>, %2 : $TensorHandle<Float>):
// CHECK-NEXT:  %3 = builtin "__tfop_Add,tt:t"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:  %4 = builtin "__tfop_Select,ttt:t"(%1 : $TensorHandle<Bool>, %3 : $TensorHandle<Float>, %2 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT: %5 = builtin "__tfop_Mul,tt:t"(%4 : $TensorHandle<Float>, %2 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:  return %5 : $TensorHandle<Float>
// CHECK-NEXT:}

public func testCast(x: Tensor<Float>) -> Tensor<Int32> {
  // expected-warning @-1 {{'x' implicitly copied to the accelerator}}
  return Tensor<Int32>(x+x)  // expected-note {{value used here}}
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testCast
// CHECK: sil private @{{.*}}testCast{{.*}} : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Int32> {
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK-NEXT: %1 = builtin "__tfop_Add,tt:t"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK:   %2 = metatype $@thick Int32.Type
// CHECK:   %3 = builtin "__tfop_Cast,t:t,DstT"(%1 : $TensorHandle<Float>, %2 : $@thick Int32.Type) : $TensorHandle<Int32>
// CHECK:   return %3 : $TensorHandle<Int32>



// This tests the attributes necessary to get arrays of integers and strings going.
public func testConvolution(x : Tensor<Float>, filter: Tensor<Float>) -> Tensor<Float> {
  return x.toDevice().convolved2D(withFilter: filter.toDevice(),
                       strides: [1, 2, 3, 4], padding: .same)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testConvolution
// CHECK: sil private @{{.*}}testConvolution{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>):
// CHECK-NEXT:  %2 = metatype $@thin Int.Type
// CHECK-NEXT:  %3 = integer_literal $Builtin.Int64, 1
// CHECK-NEXT:  %4 = integer_literal $Builtin.Int64, 2
// CHECK-NEXT:  %5 = integer_literal $Builtin.Int64, 3
// CHECK-NEXT:  %6 = integer_literal $Builtin.Int64, 4
// CHECK-NEXT:  %7 = string_literal utf8 "SAME"
// CHECK-NEXT:  %8 = builtin "__tfop_Conv2D,tt:t,strides$array,$elt,$elt,$elt,$elt,padding"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %2 : $@thin Int.Type, %3 : $Builtin.Int64, %4 : $Builtin.Int64, %5 : $Builtin.Int64, %6 : $Builtin.Int64, %7 : $Builtin.RawPointer) : $TensorHandle<Float>
// CHECK-NEXT:  return %8 : $TensorHandle<Float>
// CHECK-NEXT:}



// Testcase for an op that uses the $tensor and $shape modifiers.
public func testConstantArray() -> TensorHandle<Float> {
  return #tfop("Const", ":t", dtype: Float.self, value$tensor: [1.0, 2.0], value$shape: [2])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testConstantArray
// CHECK: sil private @{{.*}}testConstantArray{{.*}} : $@callee_owned () -> TensorHandle<Float> {
// CHECK: bb0:
// CHECK-NEXT:  %0 = metatype $@thin Float.Type
// CHECK-NEXT:  %1 = metatype $@thin Double.Type
// CHECK-NEXT:  %2 = float_literal $Builtin.FPIEEE64, 0x3FF0000000000000 // 1
// CHECK-NEXT:  %3 = float_literal $Builtin.FPIEEE64, 0x4000000000000000 // 2
// CHECK-NEXT:  %4 = metatype $@thin Int.Type
// CHECK-NEXT:  %5 = integer_literal $Builtin.Int64, 2
// CHECK-NEXT:  %6 = builtin "__tfop_Const,:t,dtype,value$tensor,$elt,$elt,value$shape,$elt"(%0 : $@thin Float.Type, %1 : $@thin Double.Type, %2 : $Builtin.FPIEEE64, %3 : $Builtin.FPIEEE64, %4 : $@thin Int.Type, %5 : $Builtin.Int64) : $TensorHandle<Float>
// CHECK-NEXT:  return %6 : $TensorHandle<Float>



public func testEmptyUnitsArray() {
  let y = Tensor<Int>(shape: [0, 20, 30], units: [])
  _ = y+y
}

/*
 CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testEmptyUnitsArray
 CHECK: sil private @{{.*}}testEmptyUnitsArray{{.*}} : $@callee_owned () -> () {
 CHECK: bb0:
 CHECK: integer_literal $Builtin.Int64, 0
 CHECK: integer_literal $Builtin.Int64, 20
 CHECK: integer_literal $Builtin.Int64, 30
 CHECK:  builtin "__tfop_Const,:t,value$tensor,value$shape,$elt,$elt,$elt,dtype"({{.*}} : $@thin Int.Type, {{.*}} : $@thin Int.Type, {{.*}} : $Builtin.Int64, {{.*}} : $Builtin.Int64, {{.*}} : $Builtin.Int64, {{.*}} : $@thin Int.Type) : $TensorHandle<Int>
 CHECK:  builtin "__tfop_Add,tt:t"({{.*}} : $TensorHandle<Int>, {{.*}} : $TensorHandle<Int>) : $TensorHandle<Int>
 */


// Sigmoid shouldn't cause copies.  This should compile with no copy warnings/errors.
public func testSigmoid(x: Tensor<Float>, y: Tensor<Float>) -> (Tensor<Float>, Tensor<Float>) {
  let a = sigmoid(x.toDevice()).toHost()
  let b = sigmoid(y.toDevice()).toHost()
  return (a, b)
}

// Likewise, mean and max shouldn't cause send/receive errors.
// FIXME: Remove #if when fixed.
#if false
public func testMeanMax(x: Tensor<Float>) -> Float {
  let a = x.mean()
  let b = x.max()
  return a+b
}
#endif
