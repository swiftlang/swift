// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import _Differentiation

var FunctionTypeMetadataTests = TestSuite("FunctionTypeMetadata")

if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
  FunctionTypeMetadataTests.test("Reflect differentiable function type") {
    expectEqual(
      "@differentiable(reverse) (Swift.Float) -> Swift.Float",
      String(reflecting: (@differentiable(reverse) (Float) -> Float).self))
    expectEqual(
      "@differentiable(reverse) (inout Swift.Float) -> ()",
      String(reflecting: (@differentiable(reverse) (inout Float) -> Void).self))
    expectEqual(
      """
      @differentiable(reverse) (Swift.Array<Swift.Float>) -> \
      Swift.Array<Swift.Float>
      """,
      String(reflecting: (@differentiable(reverse) ([Float]) -> [Float]).self))
    expectEqual(
      """
      @differentiable(reverse) (Swift.Optional<Swift.Float>) -> \
      Swift.Optional<Swift.Float>
      """,
      String(reflecting: (@differentiable(reverse) (Float?) -> Float?).self))
    // FIXME(rdar://75916833): Mangle '@noDerivative' in function types.
    // expectEqual(
    //   """
    //   @differentiable(reverse) (Swift.Optional<Swift.Float>, \
    //   @noDerivative Swift.Int) -> Swift.Optional<Swift.Float>
    //   """,
    //   String(reflecting: (
    //       @differentiable(reverse) (Float?, @noDerivative Int) -> Float?).self))
  }
}

// FIXME(rdar://75916878): Investigate why reflecting differentiable function
// types that contain generic parameters will lose '@differentiable' annotation.
// FunctionTypeMetadataTests.test("Reflect generic differentiable function type") {
//   func testGeneric<T: Differentiable>(_ type: T.Type) {
//     expectEqual(
//       """
//       @differentiable(reverse) (\(String(reflecting: type))) -> \
//       \(String(reflecting: type))
//       """,
//       String(reflecting: (@differentiable(reverse) (T) -> T).self))
//   }
//   testGeneric(Double.self)
//   testGeneric([Float].self)
//   testGeneric(Float?.self)
// }

runAllTests()
