// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Test `Parameterized` protocol.

import TensorFlow
import StdlibUnittest

var ParameterizedTests = TestSuite("Parameterized")

ParameterizedTests.test("ParameterUpdate") {
  struct Foo : Parameterized {
    @TFParameter var w = Tensor<Float>(1)
    mutating func foo() {
      updateParameters(withGradients: Parameters(w: Tensor(1))) {
        $0 += $1
      }
    }
  }
  var f = Foo()
  f.foo()
  expectEqual(2, f.w.scalar!)
}

// Temporarily disabled because `Array` no longer conforms to new `Parameterized` protocol.
/*
ParameterizedTests.test("ArrayParameterUpdate") {
  struct Foo : Parameterized {
    @TFParameter var w = [Tensor<Float>(1), Tensor<Float>(2)]
    mutating func foo() {
      updateParameters(withGradients: Parameters(w: [Tensor(1), Tensor(2)])) {
        $0 += $1
      }
    }
  }
  var f = Foo()
  f.foo()
  expectEqual(2, f.w[0].scalar!)
  expectEqual(4, f.w[1].scalar!)
}
*/

ParameterizedTests.test("AllStoredPropertiesAreParameters") {
  struct Model : Parameterized {
    // Note: Using `Tensor`-typed parameters is blocked by conforming
    // synthesized `Parameters` to `TensorArrayProtocol` for GPE.
    @TFParameter var w: Float

    // FIXME: This should be synthesized but currently doesn't work because of
    // the dependency between `Differentiable` and `Parameterized` synthesis.
    // We want `TangentVector = CotangentVector = Parameters`, but
    // `Differentiable` synthesis happens first. Investigate later.
    func tangentVector(from cotangent: Parameters) -> Parameters {
      return cotangent
    }
  }

  // TODO: Test the following `Parameters` protocol derived conformances:
  // - `VectorNumeric`
  // - `KeyPathIterable`
  // - `ParameterGroup`

  @differentiable
  func testMemberAccess(x: Model) -> Float {
    return x.w + 1
  }
  let pb = pullback(at: Model(w: 1)) { model in
    return model.w + 1
  }
  expectEqual(Model.Parameters(w: 1), pb(1))
  expectEqual(Model.Parameters(w: 3), pb(3))
}

// Test type with non-`@TFParameter` stored properties.
ParameterizedTests.test("NotAllStoredPropertiesAreParameters") {
  struct Model : Parameterized {
    @TFParameter var w: Float
    var flag: Bool

    // FIXME: This should be synthesized but currently doesn't work because of
    // the dependency between `Differentiable` and `Parameterized` synthesis.
    // We want `TangentVector = CotangentVector = Parameters`, but
    // `Differentiable` synthesis happens first. Investigate later.
    func tangentVector(from cotangent: Parameters) -> Parameters {
      return cotangent
    }
  }

  @differentiable
  func testMemberAccess(x: Model) -> Float {
    return x.w + 1
  }
  let pb = pullback(at: Model(w: 1, flag: true)) { model in
    return model.w + 1
  }
  expectEqual(Model.Parameters(w: 1), pb(1))
  expectEqual(Model.Parameters(w: 3), pb(3))
}

runAllTests()
