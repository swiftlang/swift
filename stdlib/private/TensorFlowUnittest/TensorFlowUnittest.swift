import CTensorFlow
import TensorFlow
import StdlibUnittest

// Checks if an optional OpaquePointer representing a CTFStatus has value TF_OK.
public func checkOk(_ s: OpaquePointer?, file: StaticString = #file,
                    line: UInt = #line) {
  assert(TF_GetCode(s) == TF_OK,
         String(cString: TF_Message(s)),
         file: file, line: line)
}

/// Determines if two floating point numbers are very nearly equal.
public func expectNearlyEqual<T : FloatingPoint & ExpressibleByFloatLiteral>(
  _ lhs: T, _ rhs: T, byError error: T = 0.000001
) {
  expectLT(abs(lhs - rhs), error)
}

/// Determines if two collections of floating point numbers are very nearly
/// equal.
public func expectPointwiseNearlyEqual<T, C1, C2>(
  _ lhs: C1, _ rhs: C2, byError error: T = 0.000001
) where T : FloatingPoint & ExpressibleByFloatLiteral,
  C1 : Collection, C2 : Collection, C1.Element == T, C1.Element == C2.Element {
  precondition(lhs.count == rhs.count, "Scalar count mismatch.")
  for (l, r) in zip(lhs, rhs) {
    expectNearlyEqual(l, r, byError: error)
  }
}

// The following methods help keep the SIL code of our test cases simple.
@inline(never)
public func expectNearlyEqualWithScalarTensor<T : FloatingPoint & ExpressibleByFloatLiteral>(
  _ expectedValue: T, _ x: Tensor<T>) {
  expectNearlyEqual(expectedValue, x.scalar!)
}

@inline(never)
public func expectEqualWithScalarTensor<T : Comparable & ExpressibleByIntegerLiteral>(
  _ expectedValue: T, _ x: Tensor<T>) {
  expectEqual(expectedValue, x.scalar!)
}

@inline(never)
public func printT<Scalar>(_ x: Tensor<Scalar>) {
  print(x)
}

@inline(never)
public func printT(_ message: String) {
  print(message)
}

extension TestSuite {
  static let willTargetGPU: Bool =
    CommandLine.arguments.contains("--target=gpu")
  // If the macro TPU is not specified, run the tests with CPU or GPU. Otherwise
  // use TPU.
  // For GPU execution, TensorFlow must have been built with --config=cuda,
  // and a gpu device must be available.
  @inline(never)
  public func testAllBackends(_ name: String, _ body: @escaping () -> Void) {
#if !TPU
    testCPUOrGPU(name, body)
#else
    testTPU(name, body)
#endif // TPU
  }
  public func testCPUOrGPU(_ name: String, _ body: @escaping () -> Void) {
#if !TPU
    test(name + (TestSuite.willTargetGPU ? "_GPU" : "_CPU")) {
      _RuntimeConfig.executionMode = .auto
      _RuntimeConfig.usesTFEagerAPI = true
      _RuntimeConfig.gpuMemoryAllowGrowth = true
      _RuntimeConfig.printsDebugLog = false
      withDevice(TestSuite.willTargetGPU ? .gpu : .cpu) {
        body()
      }
    }
#endif // TPU
  }
  public func testTPU(_ name: String, _ body: @escaping () -> Void) {
#if TPU
    test(name + "_TPU") {
      _RuntimeConfig.executionMode = .tpu
      _RuntimeConfig.printsDebugLog = false
      body()
    }
#endif // TPU
  }
}
