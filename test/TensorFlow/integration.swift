// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s | %FileCheck %s
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
// CHECK-NEXT:   %2 = builtin "__tfop_Add,$in,$in"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %3 = builtin "__tfop_Sub,$in,$in"(%2 : $TensorHandle<Float>, %2 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %4 = builtin "tensorflowSend_0"<TensorHandle<Float>>(%3 : $TensorHandle<Float>) : $()
// CHECK-NEXT:   %5 = builtin "__tfop_Add,$in,$in"(%1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
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
// CHECK: sil private @{{.*}}testScalar{{.*}} : $@callee_owned (TensorHandle<Builtin.FPIEEE32>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Builtin.FPIEEE32>):
// CHECK-NEXT:   %1 = unchecked_ref_cast %0 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK-NEXT:   %2 = float_literal $Builtin.FPIEEE32, 0x3F800000 // 1
// CHECK-NEXT:   %3 = integer_literal $Builtin.Int32, 1
// CHECK-NEXT:   %4 = builtin "__tfop_Const,dtype$dtype,value$tensor"(%3 : $Builtin.Int32, %2 : $Builtin.FPIEEE32) : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:   %5 = unchecked_ref_cast %4 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK-NEXT:   %6 = builtin "__tfop_Add,$in,$in"(%1 : $TensorHandle<Float>, %5 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   %7 = builtin "__tfop_Add,$in,$in"(%6 : $TensorHandle<Float>, %6 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   return %7 : $TensorHandle<Float>
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
// CHECK-NEXT:   %0 = float_literal $Builtin.FPIEEE32, 0x3F800000 // 1
// CHECK-NEXT:   %1 = integer_literal $Builtin.Int32, 1
// CHECK-NEXT:   %2 = builtin "__tfop_Const,dtype$dtype,value$tensor"(%1 : $Builtin.Int32, %0 : $Builtin.FPIEEE32) : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:   %3 = unchecked_ref_cast %2 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float> // users: %4, %4
// CHECK-NEXT:   %4 = builtin "__tfop_Add,$in,$in"(%3 : $TensorHandle<Float>, %3 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:   return %4 : $TensorHandle<Float>
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
// CHECK:  builtin "__tfop_Const
// CHECK:  cond_br {{.*}}, bb2, bb1

// CHECK:      bb1:
// CHECK-NEXT:   builtin "__tfop_Add,$in,$in"(
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
// CHECK-NEXT:    builtin "__tfop_Add,$in,$in"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
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
// CHECK-NEXT: integer_literal $Builtin.Int64, 0
// CHECK-NEXT: integer_literal $Builtin.Int32, 9
// CHECK-NEXT: builtin "__tfop_Const,dtype$dtype,value$tensor"(
// CHECK-NEXT: builtin "__tfop_Add,$in,$in"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>)
// CHECK-NEXT: builtin "tf_tensor_to_i1"(
// CHECK-NEXT: cond_br {{.*}}, bb2, bb1

// CHECK: bb3([[A:%.*]] : $TensorHandle<Float>, [[COUNT:%.*]] : $TensorHandle<Builtin.Int64>):
// CHECK-NEXT:  [[NEXTA:%.*]] = builtin "__tfop_Sub,$in,$in"([[A]] : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:  [[NEXTCOUNT:%.*]] = builtin "__tfop_Add,$in,$in"([[COUNT]] : $TensorHandle<Builtin.Int64>,
// CHECK-NEXT: [[CONDT:%.*]] = builtin "__tfop_Less,$in,$in"([[NEXTCOUNT]] : $TensorHandle<Builtin.Int64>,
// CHECK-NEXT:   [[COND:%.*]] = builtin "tf_tensor_to_i1"([[CONDT]] : $TensorHandle<Builtin.Int1>) : $Builtin.Int1
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
  // expected-warning @-1 {{'a' implicitly copied to the accelerator, use .toDevice() to make transfer explicit}}

  // expected-note @+1 {{value used here}}
  let x = Tensor<Float>(a) + Tensor<Float>(1.0) // expected-warning {{value implicitly copied to the host}} expected-error {{GraphGen cannot lower a 'send' to the host yet}}
  let y = x.scalar! + 2.0 // expected-note {{value used here}}
  // expected-warning @-1 {{value implicitly copied to the accelerator}}

  let z = Tensor<Float>(y)
  return z+z
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}scalar_manipulation{{.*}}
// CHECK: sil private @{{.*}}scalar_manipulation{{.*}} : $@callee_owned (TensorHandle<Builtin.FPIEEE32>) -> TensorHandle<Float> {
// CHECK: bb0(%0 : $TensorHandle<Builtin.FPIEEE32>):
// CHECK-NEXT:  %1 = unchecked_ref_cast %0 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float> // user: %6
// CHECK-NEXT:  %2 = float_literal $Builtin.FPIEEE32, 0x3F800000 // 1
// CHECK-NEXT:  %3 = integer_literal $Builtin.Int32, 1
// CHECK-NEXT:  %4 = builtin "__tfop_Const,dtype$dtype,value$tensor"(%3 : $Builtin.Int32, %2 : $Builtin.FPIEEE32) : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:  %5 = unchecked_ref_cast %4 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>

// CHECK-NEXT:  %6 = builtin "__tfop_Add,$in,$in"(%1 : $TensorHandle<Float>, %5 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:  %7 = builtin "tensorflowSend_1"<TensorHandle<Float>>(%6 : $TensorHandle<Float>) : $()
// CHECK-NEXT:  %8 = float_literal $Builtin.FPIEEE32, 0x40000000 // 2
// CHECK-NEXT:  %9 = integer_literal $Builtin.Int32, 1
// CHECK-NEXT:  %10 = builtin "__tfop_Const,dtype$dtype,value$tensor"(%9 : $Builtin.Int32, %8 : $Builtin.FPIEEE32) : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:  %11 = builtin "tensorflowReceive_0"<TensorHandle<Builtin.FPIEEE32>>() : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:  %12 = builtin "__tfop_Add,$in,$in"(%11 : $TensorHandle<Builtin.FPIEEE32>, %10 : $TensorHandle<Builtin.FPIEEE32>) : $TensorHandle<Builtin.FPIEEE32>
// CHECK-NEXT:  %13 = unchecked_ref_cast %12 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK-NEXT:  %14 = builtin "__tfop_Add,$in,$in"(%13 : $TensorHandle<Float>, %13 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK-NEXT:  return %14 : $TensorHandle<Float>
// CHECK-NEXT:}



public func testCast(x: Tensor<Float>) -> Tensor<Int32> {
  // expected-warning @-1 {{'x' implicitly copied to the accelerator}}
  return Tensor<Int32>(x+x)  // expected-note {{value used here}}
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testCast
// CHECK: sil private @{{.*}}testCast{{.*}} : $@callee_owned (TensorHandle<Float>) -> TensorHandle<Int32> {
// CHECK: bb0(%0 : $TensorHandle<Float>):
// CHECK-NEXT: %1 = builtin "__tfop_Add,$in,$in"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) : $TensorHandle<Float>
// CHECK:   %2 = metatype $@thick Int32.Type
// CHECK:   %3 = builtin "__tfop_Cast,$in,DstT"(%1 : $TensorHandle<Float>, %2 : $@thick Int32.Type) : $TensorHandle<Int32>
// CHECK:   return %3 : $TensorHandle<Int32>



// expected-warning @+1 2 {{implicitly copied to the accelerator}}
public func testInputListArguments(a: TensorHandle<Float>, b: Tensor<Float>) -> Tensor<Float> {
  // Pack takes an input list, not multiple inputs.  Here we're checking that
  // we can pass in an array of Tensor's and an array of TensorHandle's.
  let x: Tensor<Float> = #tfop("Pack", [a, a, a])  // expected-note {{value used here}}
  let y: Tensor<Float> = #tfop("Pack", [b, b, b])  // expected-note {{value used here}}
  return (x+y).toHost()
}

/*
 CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testInputListArguments
 CHECK: sil private @{{.*}}testInputListArguments{{.*}} : $@callee_owned (TensorHandle<Float>, TensorHandle<Float>) -> TensorHandle<Float> {
 CHECK: bb0(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>):
 CHECK:   %2 = metatype $@thin TensorHandle<Float>.Type
 CHECK:   %3 = builtin "__tfop_Pack,$in,$inelt,$inelt,$inelt"(%2 : $@thin TensorHandle<Float>.Type, %0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) : $TensorHandle<Float>
 CHECK:  %4 = metatype $@thin Tensor<Float>.Type
 CHECK:  %5 = builtin "__tfop_Pack,$in,$inelt,$inelt,$inelt"(%4 : $@thin Tensor<Float>.Type, %1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) : $TensorHandle<Float>
 CHECK:  %6 = builtin "__tfop_Add,$in,$in"(%3 : $TensorHandle<Float>, %5 : $TensorHandle<Float>) : $TensorHandle<Float>
 CHECK:  return %6 : $TensorHandle<Float>
 CHECK: }
*/


// This should produce exactly one live out value in the call to
// _swift_tfc_FinishTensorComputation.
public func liveOutTest(a : Tensor2D<Float>, b : Tensor2D<Float>,
                        c : Tensor2D<Float>) -> Tensor2D<Float> {
  return a+b+c  // expected-warning 3 {{value implicitly copied to the accelerator}}
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
 CHECK: sil  [thunk] [always_inline] @{{.*}}liveOutTest{{.*}} : $@convention(thin) (@owned Tensor2D<Float>, @owned Tensor2D<Float>, @owned Tensor2D<Float>) -> @owned Tensor2D<Float> {

 // [[RESULTBUF:%.*]] = alloc_stack $OpaquePointer
 // [[RESULTACCESS:%.*]] = begin_access [modify] [static] [[RESULTBUF]] : $*OpaquePointer
 // [[RESULTPTR:%.*]] = address_to_pointer [[RESULTACCESS]] : $*OpaquePointer to $Builtin.RawPointer
 // [[RESULTMP:%.*]] = struct $UnsafeMutablePointer<OpaquePointer> ([[RESULTPTR]] : $Builtin.RawPointer)
 // [[FINISHFN:%.*]] = function_ref @_swift_tfc_FinishTensorComputation : $@convention(thin) (@owned _TensorComputation, UnsafeMutablePointer<OpaquePointer>, Int) -> ()
 // %53 = apply [[FINISHFN]]({{.*}}, [[RESULTMP]], {{.*}}) : $@convention(thin) (@owned _TensorComputation, UnsafeMutablePointer<OpaquePointer>, Int) -> ()
*/



// Here ".mean()" contains a tensor2scalar operation, and we then convert that
// scalar back to a tensor.  This checks to make sure that tf-partition can pull
// this whole mess in graph without leaving anything on the host that will cause
// a send/receive.
public func tensorToScalarToTensor(a : Tensor<Int32>) -> Tensor<Int32> {
  let scalar = a.toDevice().mean()
  let b = Tensor(scalar)
  return (b+b).toHost()
}
