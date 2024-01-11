// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// Would fail due to unavailability of swift_autoDiffCreateLinearMapContext.

import StdlibUnittest
import DifferentiationUnittest

var AddressOnlyTangentVectorTests = TestSuite("AddressOnlyTangentVector")

// TF-1149: Test loadable class type with an address-only `TangentVector` type.

AddressOnlyTangentVectorTests.test("LoadableClassAddressOnlyTangentVector") {
  final class LoadableClass<T: Differentiable>: Differentiable {
    @differentiable(reverse)
    var stored: T

    @differentiable(reverse)
    init(_ stored: T) {
      self.stored = stored
    }

    @differentiable(reverse)
    func method(_ x: T) -> T {
      stored
    }
  }

  @differentiable(reverse)
  func projection<T: Differentiable>(_ s: LoadableClass<T>) -> T {
    var x = s.stored
    return x
  }
  expectEqual(.init(stored: 1), gradient(at: LoadableClass<Float>(10), of: projection))

  @differentiable(reverse)
  func tuple<T: Differentiable>(_ s: LoadableClass<T>) -> T {
    var tuple = (s, (s, s))
    return tuple.1.0.stored
  }
  expectEqual(.init(stored: 1), gradient(at: LoadableClass<Float>(10), of: tuple))

  @differentiable(reverse)
  func conditional<T: Differentiable>(_ s: LoadableClass<T>) -> T {
    var tuple = (s, (s, s))
    // TODO: cannot use literal `false` because it crashes
    if 1 == 0 {}
    return tuple.1.0.stored
  }
  expectEqual(.init(stored: 1), gradient(at: LoadableClass<Float>(10), of: conditional))

  @differentiable(reverse)
  func loop<T: Differentiable>(_ array: [LoadableClass<T>]) -> T {
    var result: [LoadableClass<T>] = []
    for i in withoutDerivative(at: array.indices) {
      result.append(array[i])
    }
    return result[0].stored
  }
  expectEqual([.init(stored: 1)], gradient(at: [LoadableClass<Float>(10)], of: loop))

  @differentiable(reverse)
  func arrayLiteral<T: Differentiable>(_ s: LoadableClass<T>) -> T {
    var result: [[LoadableClass<T>]] = [[s, s]]
    return result[0][1].stored
  }
  expectEqual(.init(stored: 1), gradient(at: LoadableClass<Float>(10), of: arrayLiteral))
}

runAllTests()
