// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -Xllvm -tf-module-level-graph=false -verify %s | %FileCheck %s
import TensorFlow

// FIXME: This should not build with -O.

public func trivialAdd(a: Tensor<Float>) -> Tensor<Float> {
  let b = a.toAccelerator()
  return b+b
}

/*
CHECK-LABEL: --- INPUT FUNCTION {{.*}}trivialAdd
CHECK: graph_op "Add"({{.*}} : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>) {T$dtype: i32 1, __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Float>

CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}trivialAdd
CHECK:      bb0(%0 : @unowned $TensorHandle<Float>):
CHECK-NEXT:   %1 = graph_op "Add"(%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>) {T$dtype: i32 1, __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Float>
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
 CHECK: [[A:%.*]] = graph_op "Const"() {dtype$dtype: i32 3, value$tensor: i32 0
 CHECK: [[B:%.*]] = graph_op "Const"
 CHECK: [[C:%.*]] = graph_op "Const"
 CHECK: [[RESULT:%.*]] = graph_op "OneHot"(%0 : $TensorHandle<Int32>, [[A]] : $TensorHandle<Int32>, [[B]] : $TensorHandle<Float>, [[C]] : $TensorHandle<Float>) {T$dtype: i32 1, TI$dtype: i32 3, axis: i64 1, __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Float>
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
CHECK: = graph_op "Pack"([%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>]) {__device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Float>
CHECK: graph_op "Pack"([{{.*}} : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>]) {__device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Float>
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
CHECK: bb0(%0 : @unowned $TensorHandle<Float>):
CHECK:   %1 = graph_op "Pack"([%0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>, %0 : $TensorHandle<Float>])
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
 CHECK: graph_op "foo"() {attr1: "", attr2: "abc", __device: "/job:localhost/replica:0/task:0/device:CPU:0"}
*/

public func tensorShape() -> Tensor<Float> {
  let shape : TensorShape = [1, 2]

  return Tensor(handle: #tfop("Const", dtype$dtype: Float.tensorFlowDataType, value$tensor: [17.0 as Float, 18.0], shape$shape: shape))
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
 * CHECK: graph_op "Const"() {dtype$dtype: i32 1, value$tensor: [$Float: (f32 0x3F800000 /* 1 */)], shape$shape: [$Int32: i32 1]
 * CHECK: [[B1X:%.*]] = graph_op "Const"() {dtype$dtype: i32 3, value$tensor: [$Int32: (i32 1)], shape$shape: [$Int32: i32 1],
 * CHECK: [[BX2:%.*]] = graph_op "Const"() {dtype$dtype: i32 1, value$tensor: f32 0x3F800000 /* 1 */
 * CHECK:  graph_op "Fill"([[B1X]] : $TensorHandle<Int32>, [[BX2]] : $TensorHandle<Float>)
 * CHECK: [[C1X:%.*]] = graph_op "Const"() {dtype$dtype: i32 3, value$tensor: [$Int32: (i32 1)], shape$shape: [$Int32: i32 1],
 * CHECK: [[CX2:%.*]] = graph_op "Const"() {dtype$dtype: i32 1, value$tensor: f32 0x3F800000 /* 1 */
 * CHECK:  graph_op "Fill"([[C1X]] : $TensorHandle<Int32>, [[CX2]] : $TensorHandle<Float>)
 * CHECK: graph_op "Const"() {dtype$dtype: i32 1, value$tensor: [$Float: (f32 0x3F800000 /* 1 */), (f32 0x40000000 /* 2 */), (f32 0x40400000 /* 3 */), (f32 0x40800000 /* 4 */)], shape$shape: [$Int32: (i32 2), (i32 2)],
 * CHECK-LABEL: ---- END OF
*/


public func testConvolution(x: Tensor<Float>, filter: Tensor<Float>) -> Tensor<Float> {
  return x.toAccelerator().convolved2D(withFilter: filter.toAccelerator(),
                                       strides: (1, 2, 3, 4), padding: .same)
}

/* CHECK-LABEL: ---- INPUT FUNCTION {{.*}}testConvolution
 * CHECK: graph_op "Conv2D"({{.*}} : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>) {T$dtype: i32 1, strides: [$Int32: (i32 1), (i32 2), (i32 3), (i32 4)], use_cudnn_on_gpu: i1 -1, padding: "SAME", data_format: "NHWC", dilations: [$Int32: (i32 1), (i32 1), (i32 1), (i32 1)],
 * CHECK-LABEL: ---- END OF
*/


// SR-8463: SimpleDataset itself is not a const, but the `elementShape` field
// is, so it can be const-evaluated.
struct SimpleDataset {
  let handle: VariantHandle
  let elementShape: TensorShape

  init(handle: VariantHandle, elementShape: TensorShape) {
    self.handle = handle
    self.elementShape = elementShape
  }
}

public func testShapeList() {
  let t = Tensor<Int32>([0])
  let handle: VariantHandle = #tfop(
    "TensorSliceDataset", [t],
    Toutput_types$dtype: [Int32.tensorFlowDataType],
    output_shapes: [TensorShape()]
  )
  let dataset = SimpleDataset(handle: handle, elementShape: TensorShape())
  _ = #tfop("AnonymousIterator",
            output_types$dtype: [Int32.tensorFlowDataType],
            output_shapes: [dataset.elementShape]) as ResourceHandle
}

// Another test case for SR-8463.
struct SimpleIterator {
  let handle: ResourceHandle
  let elementShape: TensorShape

  mutating func next() -> Tensor<Float> {
    return #tfop("IteratorGetNext", handle,
                 output_types$dtype: [Int32.tensorFlowDataType],
                 output_shapes: [elementShape])
  }
}

public func testShapeList2() {
  let t = Tensor<Int32>([0])
  let handle: VariantHandle = #tfop(
    "TensorSliceDataset", [t],
    Toutput_types$dtype: [Int32.tensorFlowDataType],
    output_shapes: [TensorShape()]
  )
  let dataset = SimpleDataset(handle: handle, elementShape: TensorShape())
  let resource = #tfop("AnonymousIterator",
                       output_types$dtype: [Int32.tensorFlowDataType],
                       output_shapes: [dataset.elementShape]) as ResourceHandle

  var it = SimpleIterator(handle: resource, elementShape: dataset.elementShape)
  _ = it.next()
}

// CHECK-LABEL: ---- INPUT FUNCTION {{.*}}testShapeList
// CHECK: graph_op "AnonymousIterator"() {output_types$dtype: [$TensorDataType: (((i32 3)))], output_shapes: [$TensorShape: ([$Int32: ])]

@TensorFlowGraph
func isZero(_ x: Tensor<Float>) -> Tensor<Bool> {
  return x.elementsEqual(Tensor<Float>(0));
}

public func noescapeFuncAsAttr(_ f: @convention(tensorflow) (Tensor<Float>) -> Tensor<Bool>) {
  let t = Tensor<Int32>([0])
  let dataset: VariantHandle = #tfop(
    "TensorSliceDataset", [t],
    Toutput_types$dtype: [Int32.tensorFlowDataType],
    output_shapes: [TensorShape()]
  )
  let _: VariantHandle = #tfop(
    "FilterDataset", dataset, [Tensor<Int32>(0)], predicate: isZero,
    Targuments$dtype: [Int32.tensorFlowDataType], output_types$dtype: [Float.tensorFlowDataType], output_shapes: [TensorShape()]
  )
}
// Ensure that private functions are partitioned and lowered
// if they are referred to in graph ops as attributes.
// CHECK-LABEL:--- TFPartition Accelerator Result: {{.*}}isZero{{.*}}
// CHECK: bb0(%0 : @unowned $TensorHandle<Float>):
// CHECK:  [[A:%.*]] = graph_op "Const"()
// CHECK:  [[B:%.*]] = graph_op "Equal"(%0 : $TensorHandle<Float>, %1 : $TensorHandle<Float>) {T$dtype: i32 1, __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Bool>
// CHECK:  return [[B]] : $TensorHandle<Bool>                 // id: %3

// CHECK-LABEL: ---- INPUT FUNCTION {{.*}}noescapeFuncAsAttr
// CHECK: graph_op "FilterDataset"(%{{.*}} : $VariantHandle, [%{{.*}} : $TensorHandle<Int32>]) {predicate: @{{.*}}isZero{{.*}} : $@convention(tensorflow) (@guaranteed Tensor<Float>) -> @owned Tensor<Bool>

// Support higher-order functions that process tensors.
// foo() is not a partitionable function.
public func foo(tfOp: () -> Tensor<Float>) {
  let _ = tfOp()
}

// Deabstraction should inline foo into bar().
public func bar() {
  foo(tfOp: { return Tensor<Float>(1.0) })
}

// CHECK-LABEL: --- INPUT FUNCTION {{.*}}bar
// CHECK: graph_op "Const"

// Check that tensorflow convention is propagated to closures from declarations.
public func testTensorFlowClosures(_ a: Float) -> Tensor<Int32>{
  let closure: @convention(tensorflow) (Tensor<Float>) -> Tensor<Int32> = {
    return Tensor<Int32>($0)
  }
  let f = Tensor<Float>(a);
  return closure(f);
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testTensorFlowClosures{{.*}}
// CHECK: sil private {{.*}}testTensorFlowClosures{{.*}} : $@callee_owned (TensorHandle<Builtin.FPIEEE32>) -> TensorHandle<Int32> {
// CHECK: bb0(%0 : @unowned $TensorHandle<Builtin.FPIEEE32>):
// CHECK:  [[A:%.*]] = unchecked_ref_cast %0 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
// CHECK:  [[B:%.*]] = graph_op "Cast"([[A]] : $TensorHandle<Float>) {SrcT$dtype: i32 1, DstT$dtype: i32 3, __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Int32>
// CHECK:  return [[B]] : $TensorHandle<Int32>
// CHECK: } 

// CHECK-LABEL --- TFPartition Accelerator Result: [[NAME:*.*]]
// sil private @[[NAME]] : $@callee_owned (TensorHandle<Builtin.FPIEEE32>) -> TensorHandle<Int32> {
// bb0(%0 : @unowned $TensorHandle<Builtin.FPIEEE32>):
//   [[A:%.*]] = unchecked_ref_cast %0 : $TensorHandle<Builtin.FPIEEE32> to $TensorHandle<Float>
//   [[B:%.*]] = graph_op "Cast"([[A]] : $TensorHandle<Float>) {SrcT$dtype: i32 1, DstT: $Int32, __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $TensorHandle<Int32>
//   return [[B]] : $TensorHandle<Int32>
// } // end sil function '[[NAME]]'

public func testExtractDTypeList() {
  struct Foo { let a, b: Tensor<Float> }
  let elements = Foo(a: Tensor([1]), b: Tensor([2]))
  // TODO: Support heterogeneous input lists so that we can replace b's dtype with something else.
  // TODO: Support unpacking a struct value into an input list so that we can replace
  // `[elements.a, elements.b]` with `elements`.
  let _: VariantHandle = #tfop("TensorSliceDataset", [elements.a, elements.b],
                               Toutput_types$typeList: Foo.self,
                               output_shapes: [TensorShape()])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testExtractDTypeList{{.*}}
// CHECK: graph_op "TensorSliceDataset"([{{.*}} : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>]) {Toutput_types$dtype: [$UInt32: i32 1, i32 1], output_shapes: [$TensorShape: ([$Int32: ])], __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $VariantHandle

public func testExtractTupleDTypeList() {
    let bar: (a: Tensor<Int32>, b: Tensor<Int32>) = (Tensor(0), Tensor(1))
    let _: VariantHandle = #tfop("TensorSliceDataset", [bar.a, bar.b],
                                 Toutput_types$typeList: type(of: bar),
                                 output_shapes: [TensorShape()])
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testExtractTupleDTypeList{{.*}}
// CHECK: graph_op "TensorSliceDataset"([{{.*}} : $TensorHandle<Int32>, {{.*}} : $TensorHandle<Int32>]) {Toutput_types$dtype: [$UInt32: i32 3, i32 3], output_shapes: [$TensorShape: ([$Int32: ])], __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $VariantHandle

public func testExtractUnknownShapeList() {
  struct Foo { let a, b: Tensor<Float> }
  let elements = Foo(a: Tensor([1]), b: Tensor([2]))
  let _: VariantHandle = #tfop("TensorSliceDataset", [elements.a, elements.b],
                               Toutput_types$dtype: [Float.tensorFlowDataType, Float.tensorFlowDataType],
                               output_shapes$unknownShapeList: Foo.self)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}testExtractUnknownShapeList{{.*}}
// CHECK: graph_op "TensorSliceDataset"([{{.*}} : $TensorHandle<Float>, {{.*}} : $TensorHandle<Float>]) {Toutput_types$dtype: [$TensorDataType: (((i32 1))), (((i32 1)))], output_shapes: [$Optional<TensorShape>: #Optional.none!enumelt, #Optional.none!enumelt], __device: "/job:localhost/replica:0/task:0/device:CPU:0"} : $VariantHandle

// Tests that private functions operating on Tensors are partitioned
// if they are only referred in code.  In this case, addOne should be
// partitioned, but not getZero.
//
// expected-warning @+1 {{'x' implicitly copied to the accelerator}}
private func addOne(_ x : Tensor<Float>) -> Tensor<Float> {
  // expected-note @+1 {{value used here}}
  return x + Tensor<Float>(1.0)
}
private func getZero() -> Tensor<Float> { return Tensor<Float>(0.0) }

public func getTensorAndAddOneFunction() -> (Tensor<Float>, (Tensor<Float>) -> Tensor<Float>) {
  let x = Tensor<Float>(1.0)
  return (x * x, addOne)
}

// CHECK-LABEL: --- TFPartition Accelerator Result: {{.*}}addOne{{.*}}
// CHECK-NOT: --- TFPartition Accelerator Result: {{.*}}getZero{{.*}}
// CHECK: --- TFPartition Accelerator Result: {{.*}}getTensorAndAddOneFunction{{.*}}

