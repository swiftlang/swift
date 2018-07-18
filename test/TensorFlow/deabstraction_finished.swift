// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -Xllvm -tf-strict-deabstraction -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -Xllvm -tf-strict-deabstraction -verify %s | %FileCheck %s
import TensorFlow

// FIXME: This should not build with -O.

public func trivialAdd(a: Tensor<Float>) -> Tensor<Float> {
  let b = a.toAccelerator()
  return b+b
}

/*
CHECK-LABEL: --- INPUT FUNCTION {{.*}}trivialAdd
CHECK: graph_op "Add,i,i"({{.*}} : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>) {T: $Float, __device: "/device:CPU:0"} : $TensorHandle<Float>

CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}trivialAdd
CHECK:      bb0(%0 : $TensorHandle<Float>):
CHECK-NEXT:   %1 = graph_op "Add,i,i"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) {T: $Float, __device: "/device:CPU:0"} : $TensorHandle<Float>
CHECK-NEXT:   return %1 : $TensorHandle<Float>

CHECK-LABEL: --- TFPartition Host Result: {{.*}}trivialAdd
 */



// @constExpr
func one() -> Int {
  return 1
}

public func constexprCall(a: Tensor<Float>, idx: Tensor<Int32>) -> Tensor<Float> {
  return Tensor<Float>(oneHotAtIndices: idx.toAccelerator(), depth: 0, axis: one())
}

/*
 CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}constexprCall
 CHECK: [[A:%.*]] = graph_op "Const"() {dtype$dtype: $Builtin.Int64, value$tensor: i64 0
 CHECK: [[AC:%.*]] = graph_op "Cast,i"
 CHECK: [[B:%.*]] = graph_op "Const"
 CHECK: [[BC:%.*]] = graph_op "Cast,i"
 CHECK: [[C:%.*]] = graph_op "Const"
 CHECK: [[CX:%.*]] = unchecked_ref_cast [[C]] : $TensorHandle<Builtin.Int32> to $TensorHandle<Int32>
 CHECK: [[BX:%.*]] = unchecked_ref_cast [[BC]] : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
 CHECK: [[AX:%.*]] = unchecked_ref_cast [[AC]] : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
 CHECK: [[RESULT:%.*]] = graph_op "OneHot,i,i,i,i"(%0 : $TensorHandle<Int32>, [[CX]] : $TensorHandle<Int32>, [[BX]] : $TensorHandle<Float>, [[AX]] : $TensorHandle<Float>) {T: $Float, TI: $Int32, axis: i64 1, __device: "/device:CPU:0"} : $TensorHandle<Float>
  CHECK: return [[RESULT]]
*/


struct Wrapper {
  let v : Int
}

public func f(a: Tensor<Float>, idx: Tensor<Int32>) -> Tensor<Float> {
  let w = Wrapper(v: 1)
  return Tensor<Float>(oneHotAtIndices: idx.toAccelerator(), depth: 0, axis: w.v)
}


// expected-warning @+1 2 {{implicitly copied to the accelerator}}
public func testInputListArguments(a: TensorHandle<Float>, b: Tensor<Float>) -> Tensor<Float> {
  // Pack takes an input list, not multiple inputs.  Here we're checking that
  // we can pass in an array of Tensor's and an array of TensorHandle's.
  let x: TensorHandle<Float> = #tfop("Pack", [a, a, a])  // expected-note {{value used here}}
  let y: TensorHandle<Float> = #tfop("Pack", [b, b, b])  // expected-note {{value used here}}
  return (Tensor(handle: x)+Tensor(handle: y)).toHost()
}

/*
CHECK-LABEL: ---- INPUT FUNCTION {{.*}}testInputListArguments
CHECK: = graph_op "Pack,L,e,e,e"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) {__device: "/device:CPU:0"} : $TensorHandle<Float>
CHECK: graph_op "Pack,L,e,e,e"({{.*}} : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>) {__device: "/device:CPU:0"} : $TensorHandle<Float>
CHECK-LABEL: ---- END OF INPUT FUNCTION
*/

// expected-warning @+1 {{implicitly copied to the accelerator}}
public func inputListMultipleUses(a: TensorHandle<Float>)
   -> (Tensor<Float>, [TensorHandle<Float>]) {
  let arr = [a, a, a]
  let x: TensorHandle<Float> = #tfop("Pack", arr)  // expected-note {{value used here}}
  return (Tensor(handle: x).toHost(), arr)
}

/*
CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}inputListMultipleUses
CHECK: bb0(%0 : $TensorHandle<Float>):
CHECK:   %1 = graph_op "Pack,L,e,e,e"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>)
CHECK:   return %1 : $TensorHandle<Float>
CHECK-LABEL: ----
*/

public func stringAttributes() {
  let str = "abc"
  // expected-error @+1 {{op named 'foo' is not registered in TensorFlow}}
  let _ : TensorHandle<Float> = #tfop("foo", attr1: String(), attr2: str)
}
/*
CHECK-LABEL: --- INPUT FUNCTION {{.*}}stringAttributes
 CHECK: graph_op "foo"() {attr1: "", attr2: "abc", __device: "/device:CPU:0"}
*/

public func tensorShape() -> Tensor<Float> {
  let shape : TensorShape = [1, 2]

  return Tensor(handle: #tfop("Const", dtype: Float.self, value$tensor: [17.0 as Float, 18.0], value$shape: shape))
}

// b/75407624
// This requires propagation of the array initializer of TensorShape through its
// initializers.
public func test75407624() {
  let a = Tensor<Float>([1])
  let b = Tensor<Float>(shape: [1], repeating: 1)
  let c = Tensor<Float>(shape: [1], repeating: 1)
  let d = Tensor<Float>(shape: [2,2], scalars: [1,2,3,4])
  _ = a+b+c+d
}
/* CHECK-LABEL: ---- INPUT FUNCTION {{.*}}test75407624
 * CHECK: graph_op "Const"() {dtype: $Float, value$tensor: [$Float: (f32 0x3F800000 /* 1 */)], value$shape: [$Int32: i32 1]
 * CHECK: [[B1X:%.*]] = graph_op "Const"() {dtype: $Int32, value$tensor: [$Int32: (i32 1)], value$shape: [$Int32: i32 1],
 * CHECK: [[BX2:%.*]] = graph_op "tfc.scalarToTensor,s"(
 * CHECK:  graph_op "Fill,i,i"([[B1X]] : $TensorHandle<Int32>, [[BX2]] : $TensorHandle<Float>)
 * CHECK: [[C1X:%.*]] = graph_op "Const"() {dtype: $Int32, value$tensor: [$Int32: (i32 1)], value$shape: [$Int32: i32 1],
 * CHECK: [[CX2:%.*]] = graph_op "tfc.scalarToTensor,s"(
 * CHECK:  graph_op "Fill,i,i"([[C1X]] : $TensorHandle<Int32>, [[CX2]] : $TensorHandle<Float>)
 * CHECK: graph_op "Const"() {dtype: $Float, value$tensor: [$Float: (f32 0x3F800000 /* 1 */), (f32 0x40000000 /* 2 */), (f32 0x40400000 /* 3 */), (f32 0x40800000 /* 4 */)], value$shape: [$Int32: (i32 2), (i32 2)],
 * CHECK-LABEL: ---- END OF 
*/


public func testConvolution(x: Tensor<Float>, filter: Tensor<Float>) -> Tensor<Float> {
  return x.toAccelerator().convolved2D(withFilter: filter.toAccelerator(),
                                       strides: (1, 2, 3, 4), padding: .same)
}

/* CHECK-LABEL: ---- INPUT FUNCTION {{.*}}testConvolution
 * CHECK: graph_op "Conv2D,i,i"({{.*}} : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>) {T: $Float, strides: [$Int32: (i32 1), (i32 2), (i32 3), (i32 4)], use_cudnn_on_gpu: i1 -1, padding: "SAME", data_format: "NHWC", dilations: [$Int32: (i32 1), (i32 1), (i32 1), (i32 1)],
 * CHECK-LABEL: ---- END OF
*/
