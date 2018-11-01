// RUN: %target-run-disable-deabstraction-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// REQUIRES: tensorflow

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DynamicAttributeTests = TestSuite("DynamicAttribute")

// ===== Dynamic Attribute Values ====
// These global vars with @inline(never) loaders ensure that the compiler
// can't find the constant value of the attributes.

var dtypeInt32 = Int32.tensorFlowDataType
@inline(never)
func loadDtypeInt32() -> TensorDataType {
  return dtypeInt32
}

var dtypeDouble = Double.tensorFlowDataType
@inline(never)
func loadDtypeDouble() -> TensorDataType {
  return dtypeDouble
}

var stridesInt32 = (Int32(1), Int32(1), Int32(1), Int32(1))
@inline(never)
func loadStridesInt32() -> (Int32, Int32, Int32, Int32) {
  return stridesInt32
}

var stridesInt64 = [Int64(1), Int64(1), Int64(1), Int64(1)]
@inline(never)
func loadStridesInt64() -> [Int64] {
  return stridesInt64
}

var trueBool = true
@inline(never)
func loadTrue() -> Bool {
  return trueBool
}

var falseBool = false
@inline(never)
func loadFalse() -> Bool {
  return falseBool
}

var int32_1: Int32 = 1
@inline(never)
func loadInt32_1() -> Int32 {
  return int32_1
}

var int32_2: Int32 = 2
@inline(never)
func loadInt32_2() -> Int32 {
  return int32_2
}

var int64_1: Int64 = 1
@inline(never)
func loadInt64_1() -> Int64 {
  return int64_1
}

var int64_2: Int64 = 2
@inline(never)
func loadInt64_2() -> Int64 {
  return int64_2
}

var double_1: Double = 1
@inline(never)
func loadDouble_1() -> Double {
  return double_1
}

var double_0point1: Double = 0.1
@inline(never)
func loadDouble_0point1() -> Double {
  return double_0point1
}

var float_1: Float = 1
@inline(never)
func loadFloat_1() -> Float {
  return float_1
}

var float_0point1: Float = 0.1
@inline(never)
func loadFloat_0point1() -> Float {
  return float_0point1
}

var boundariesDouble: [Double] = [0, 1, 2, 3]
@inline(never)
func loadBoundariesDouble() -> [Double] {
  return boundariesDouble
}

var boundariesFloat: [Float] = [0, 1, 2, 3]
@inline(never)
func loadBoundariesFloat() -> [Float] {
  return boundariesFloat
}

var tensorShapeArray = [TensorShape([1]), TensorShape([2])]
@inline(never)
func loadTensorShapeArray() -> [TensorShape] {
  return tensorShapeArray
}

var optionalTensorShapeArray = [TensorShape([1]), nil]
@inline(never)
func loadOptionalTensorShapeArray() -> [TensorShape?] {
  return optionalTensorShapeArray
}

var tensorDataTypeArray = [Int32.tensorFlowDataType, Int32.tensorFlowDataType]
@inline(never)
func loadTensorDataTypeArray() -> [TensorDataType] {
  return tensorDataTypeArray
}

var VALIDString = "VALID"
@inline(never)
func loadVALIDString() -> String {
  return VALIDString
}

// ==== Convolution Helper Values ====
// Helper values for tests involving convolution ops.

let convImage = Tensor<Float>([[
  [[1], [0], [0]],
  [[0], [1], [0]],
  [[0], [0], [0]]
]])
let convFilter = Tensor<Float>([
  [[[1]], [[1]]],
  [[[1]], [[1]]]
])
let convExpectedResult = ShapedArray<Float>(
    shape: [1, 2, 2, 1],
    scalars: [2, 1,
              1, 1]
)

// ==== Actual Tests ====

DynamicAttributeTests.test("NormalAttribute Bool") {
  let input = Tensor<Int32>([[1, 2], [2, 1]])
  let reductionIndices = Tensor<Int32>(0)

  let resultKeepDimsTrue = Raw.max(input, reductionIndices: reductionIndices,
                                   keepDims: loadTrue())
  let expectedResultKeepDimsTrue = ShapedArray<Int32>(shape: [1, 2],
                                                      scalars: [2, 2])
  expectEqual(expectedResultKeepDimsTrue, resultKeepDimsTrue.array)

  let resultKeepDimsFalse = Raw.max(input, reductionIndices: reductionIndices,
                                    keepDims: loadFalse())
  let expectedResultKeepDimsFalse = ShapedArray<Int32>(shape: [2],
                                                       scalars: [2, 2])
  expectEqual(expectedResultKeepDimsFalse, resultKeepDimsFalse.array)
}

DynamicAttributeTests.test("NormalAttribute Int64") {
  let input = Tensor<Int32>([1, 2, 3, 4, 5])

  let shuffled1 = Raw.randomShuffle(value: input, seed: loadInt64_1(),
                                    seed2: loadInt64_1())
  let expectedResult1 = ShapedArray<Int32>([1, 4, 2, 3, 5])
  expectEqual(expectedResult1, shuffled1.array)

  let shuffled2 = Raw.randomShuffle(value: input, seed: loadInt64_2(),
                                    seed2: loadInt64_2())
  let expectedResult2 = ShapedArray<Int32>([1, 4, 2, 5, 3])
  expectEqual(expectedResult2, shuffled2.array)
}

DynamicAttributeTests.test("NormalAttribute Double") {
  let x = Tensor<Double>(1)
  let y = Tensor<Double>(1.5)

  let result1 = Raw.approximateEqual(x, y, tolerance: loadDouble_1())
  expectEqual(true, result1.scalar!)

  let result2 = Raw.approximateEqual(x, y, tolerance: loadDouble_0point1())
  expectEqual(false, result2.scalar!)
}

DynamicAttributeTests.test("NormalAttribute Float") {
  let x = Tensor<Float>(1)
  let y = Tensor<Float>(1.5)

  let result1: Tensor<Bool> = #tfop("ApproximateEqual", x, y,
                                    T$dtype: Float.tensorFlowDataType,
                                    tolerance: loadFloat_1())
  expectEqual(true, result1.scalar!)

  let result2: Tensor<Bool> = #tfop("ApproximateEqual", x, y,
                                    T$dtype: Float.tensorFlowDataType,
                                    tolerance: loadFloat_0point1())
  expectEqual(false, result2.scalar!)
}

DynamicAttributeTests.test("NormalAttribute String") {
  let result: Tensor<Float> = #tfop("Conv2D", convImage, convFilter,
                                    T$dtype: Float.tensorFlowDataType,
                                    strides: [1, 1, 1, 1] as [Int32],
                                    padding: loadVALIDString())
  expectEqual(convExpectedResult, result.array)
}

DynamicAttributeTests.test("NormalAttribute Array<Bool>") {
  // There aren't any ops that take bool list attributes!
}

DynamicAttributeTests.test("NormalAttribute Array<Int32>") {
  let result = convImage.convolved2D(withFilter: convFilter,
                                     strides: loadStridesInt32(),
                                     padding: .valid)
  expectEqual(convExpectedResult, result.array)
}

DynamicAttributeTests.test("NormalAttribute Array<Int64>") {
  let result: Tensor<Float> = #tfop("Conv2D", convImage, convFilter,
                                    T$dtype: Float.tensorFlowDataType,
                                    strides: loadStridesInt64(),
                                    padding: "VALID")
  expectEqual(convExpectedResult, result.array)
}

DynamicAttributeTests.test("NormalAttribute Array<Double>") {
  let input = Tensor<Double>([-1, 0.1, 4.3, 1.2])
  let result = Raw.bucketize(input, boundaries: loadBoundariesDouble())
  let expectedResult = ShapedArray<Int32>([0, 1, 4, 2])
  expectEqual(expectedResult, result.array)
}

DynamicAttributeTests.test("NormalAttribute Array<Float>") {
  let input = Tensor<Float>([-1, 0.1, 4.3, 1.2])
  let result: Tensor<Int32> = #tfop("Bucketize", input,
                                    T$dtype: Float.tensorFlowDataType,
                                    boundaries: loadBoundariesFloat())
  let expectedResult = ShapedArray<Int32>([0, 1, 4, 2])
  expectEqual(expectedResult, result.array)
}

/// Checks that `dataset` is a TensorSliceDataset of
/// Tensor<Int32>([1, 2]) and Tensor<Int32>([[1, 1], [2, 2]])
func check(dataset: VariantHandle) {
  let outputTypes = [Int32.tensorFlowDataType, Int32.tensorFlowDataType]
  let outputShapes = [nil, nil] as [TensorShape?]
  let iterator: ResourceHandle = #tfop(
    "IteratorV2", shared_name: "blah", container: "earth",
    output_types$dtype: outputTypes, output_shapes: outputShapes
  )
  #tfop("MakeIterator", dataset, iterator) as Void
  var next: (Tensor<Int32>, Tensor<Int32>) = #tfop(
    "IteratorGetNext", iterator,
    output_types$dtype: outputTypes, output_shapes: outputShapes
  )
  expectEqual(ShapedArray<Int32>([1]), next.0.array)
  expectEqual(ShapedArray<Int32>([1, 1]), next.1.array)
  next = #tfop(
    "IteratorGetNext", iterator,
    output_types$dtype: outputTypes, output_shapes: outputShapes
  )
  expectEqual(ShapedArray<Int32>([2]), next.0.array)
  expectEqual(ShapedArray<Int32>([2, 2]), next.1.array)
}

DynamicAttributeTests.test("NormalAttribute Array<TensorShape>") {
  let elements1 = Tensor<Int32>([[1], [2]])
  let elements2 = Tensor<Int32>([[1, 1], [2, 2]])
  let dataset: VariantHandle = #tfop(
    "TensorSliceDataset", elements1, elements2,
    Toutput_types$dtype: [Int32.tensorFlowDataType, Int32.tensorFlowDataType],
    output_shapes: loadTensorShapeArray()
  )
  check(dataset: dataset)
}

DynamicAttributeTests.test("NormalAttribute Array<TensorShape?>") {
  let elements1 = Tensor<Int32>([[1], [2]])
  let elements2 = Tensor<Int32>([[1, 1], [2, 2]])
  let dataset: VariantHandle = #tfop(
    "TensorSliceDataset", [elements1, elements2],
    Toutput_types$dtype: [Int32.tensorFlowDataType, Int32.tensorFlowDataType],
    output_shapes: loadOptionalTensorShapeArray()
  )
  check(dataset: dataset)
}

DynamicAttributeTests.test("TFDataTypeAttribute TensorDataType") {
  let t1 = Tensor<Int32>(-1)
  let t1Result: Tensor<Int32> = #tfop("Abs", t1, T$dtype: loadDtypeInt32())
  expectEqual(1, t1Result.scalar!)

  let t2 = Tensor<Double>(-2)
  let t2Result: Tensor<Double> = #tfop("Abs", t2, T$dtype: loadDtypeDouble())
  expectEqual(2, t2Result.scalar!)
}

DynamicAttributeTests.test("TFDataTypeAttribute Array<TensorDataType>") {
  let elements1 = Tensor<Int32>([[1], [2]])
  let elements2 = Tensor<Int32>([[1, 1], [2, 2]])
  let dataset: VariantHandle = #tfop(
    "TensorSliceDataset", [elements1, elements2],
    Toutput_types$dtype: loadTensorDataTypeArray(),
    output_shapes: [nil, nil] as [TensorShape?]
  )
  check(dataset: dataset)
}

runAllTests()
