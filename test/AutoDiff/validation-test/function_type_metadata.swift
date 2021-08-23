// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import _Differentiation

var FunctionTypeMetadataTests = TestSuite("FunctionTypeMetadata")

if #available(macOS 11.3, iOS 14.5, tvOS 14.5, watchOS 7.4, *) {
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
    expectEqual(
      """
      @differentiable(reverse) (Swift.Optional<Swift.Float>, \
      @noDerivative Swift.Int) -> Swift.Optional<Swift.Float>
      """,
      String(reflecting: (
          @differentiable(reverse)
              (Float?, @noDerivative Int) -> Float?).self))
    expectEqual(
      """
      @differentiable(reverse) (Swift.Optional<Swift.Float>, \
      __owned @noDerivative Swift.Int) -> Swift.Optional<Swift.Float>
      """,
      String(reflecting: (
          @differentiable(reverse)
              (Float?, __owned @noDerivative Int) -> Float?).self))
    expectEqual(
      """
      @differentiable(reverse) (Swift.Optional<Swift.Float>, \
      inout @noDerivative Swift.Int) -> Swift.Optional<Swift.Float>
      """,
      String(reflecting: (
          @differentiable(reverse)
              (Float?, inout @noDerivative Int) -> Float?).self))
    expectEqual(
      """
      @differentiable(reverse) @Sendable (Swift.Optional<Swift.Float>, \
      inout @noDerivative Swift.Int) -> Swift.Optional<Swift.Float>
      """,
      String(reflecting: (
          @differentiable(reverse) @Sendable
              (Float?, inout @noDerivative Int) -> Float?).self))
  }
}

if #available(macOS 12, iOS 15, tvOS 15, watchOS 8, *) {
  FunctionTypeMetadataTests.test("Reflect generic differentiable function type") {
    func testGeneric<T: Differentiable>(_ type: T.Type) {
      expectEqual(
        """
        @differentiable(reverse) (\(String(reflecting: type))) -> \
        \(String(reflecting: type))
        """,
        String(reflecting: (@differentiable(reverse) (T) -> T).self))
    }
    testGeneric(Double.self)
    testGeneric([Float].self)
    testGeneric(Float?.self)
  }
}

runAllTests()
