// RUN: %target-run-dynamic-compilation-swift --attributes-json %S/Inputs/attributes.json
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// REQUIRES: tensorflow

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest
import Python

var DynamicAttributeTests = TestSuite("DynamicAttribute")

/// Loads an attribute value from a file. This ensures that this test actually
/// exercises dynamic attributes.
func loadAttribute<T: PythonConvertible>(_ key: String) -> T {
  let attributesFilenameIndex =
      CommandLine.arguments.firstIndex(of: "--attributes-json")! + 1
  let attributesFilename = CommandLine.arguments[attributesFilenameIndex]
  let json = Python.import("json")
  let f = Python.open(attributesFilename, "r")
  defer { f.close() }
  return T(json.load(f)[PythonObject(key)])!
}

extension TensorDataType : PythonConvertible {
  public var pythonObject: PythonObject {
    fatalError("unimplemented")
  }

  public init?(_ pythonObject: PythonObject) {
    guard let str = String(pythonObject) else { return nil }
    switch str {
    case "Double":
      self = Double.tensorFlowDataType
    case "Int32":
      self = Int32.tensorFlowDataType
    default:
      return nil
    }
  }
}

DynamicAttributeTests.test("single TFDataTypeAttribute") {
  let t1 = Tensor<Int32>(-1)
  let t1Result: Tensor<Int32> = #tfop(
    "Abs", t1, T$dtype: loadAttribute("dtype_Int32") as TensorDataType)
  expectEqual(1, t1Result.scalar!)

  let t2 = Tensor<Double>(-2)
  let t2Result: Tensor<Double> = #tfop(
    "Abs", t2, T$dtype: loadAttribute("dtype_Double") as TensorDataType)
  expectEqual(2, t2Result.scalar!)
}

runAllTests()
