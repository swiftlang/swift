// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var AddressOnlyTangentVectorTests = TestSuite("AddressOnlyTangentVector")

// TF-1149: Test loadable class type with an address-only `TangentVector` type.

AddressOnlyTangentVectorTests.test("LoadableClassAddressOnlyTangentVector") {
  final class LoadableClass<T: Differentiable>: Differentiable {
    @differentiable
    var stored: T

    @differentiable
    init(_ stored: T) {
      self.stored = stored
    }

    @differentiable
    func method(_ x: T) -> T {
      stored
    }
  }

  @differentiable
  func projection<T: Differentiable>(_ s: LoadableClass<T>) -> T {
    var x = s.stored
    return x
  }
  expectEqual(.init(stored: 1), gradient(at: LoadableClass<Float>(10), in: projection))

  @differentiable
  func tuple<T: Differentiable>(_ s: LoadableClass<T>) -> T {
    var tuple = (s, (s, s))
    return tuple.1.0.stored
  }
  expectEqual(.init(stored: 1), gradient(at: LoadableClass<Float>(10), in: tuple))

  @differentiable
  func conditional<T: Differentiable>(_ s: LoadableClass<T>) -> T {
    var tuple = (s, (s, s))
    if false {}
    return tuple.1.0.stored
  }
  expectEqual(.init(stored: 1), gradient(at: LoadableClass<Float>(10), in: conditional))

  @differentiable
  func loop<T: Differentiable>(_ array: [LoadableClass<T>]) -> T {
    var result: [LoadableClass<T>] = []
    for i in withoutDerivative(at: array.indices) {
      result.append(array[i])
    }
    return result[0].stored
  }
  expectEqual([.init(stored: 1)], gradient(at: [LoadableClass<Float>(10)], in: loop))

  @differentiable
  func arrayLiteral<T: Differentiable>(_ s: LoadableClass<T>) -> T {
    var result: [[LoadableClass<T>]] = [[s, s]]
    return result[0][1].stored
  }
  expectEqual(.init(stored: 1), gradient(at: LoadableClass<Float>(10), in: arrayLiteral))
}

runAllTests()
