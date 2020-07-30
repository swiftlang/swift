// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var OptionalTests = TestSuite("OptionalDifferentiation")

//===----------------------------------------------------------------------===//
// Basic tests.
//===----------------------------------------------------------------------===//

/*
// TODO(TF-433): operator `??` lowers to `try_apply` instead of `switch_enum`,
// which is not yet supported by differentiation.
@differentiable
func optional1(_ maybeX: Float?) -> Float {
  return maybeX ?? 10
}
*/

OptionalTests.test("Let") {
  @differentiable
  func optional2(_ maybeX: Float?) -> Float {
    if let x = maybeX {
        return x * x
    }
    return 10
  }
  expectEqual(gradient(at: 10, in: optional2), .init(20.0))
  expectEqual(gradient(at: nil, in: optional2), .init(0.0))

  @differentiable
  func optional_nested2(_ nestedMaybeX: Float??) -> Float {
    if let maybeX = nestedMaybeX {
      if let x = maybeX {
        return x * x
      }
      return 10
    }
    return 10
  }
  expectEqual(gradient(at: 10, in: optional_nested2), .init(.init(20.0)))
  expectEqual(gradient(at: nil, in: optional_nested2), .init(.init(0.0)))
}

OptionalTests.test("Switch") {
  @differentiable
  func optional3(_ maybeX: Float?) -> Float {
    switch maybeX {
    case nil: return 10
    case let .some(x): return x * x
    }
  }

  expectEqual(gradient(at: 10, in: optional3), .init(20.0))
  expectEqual(gradient(at: nil, in: optional3), .init(0.0))

  @differentiable
  func optional_nested3(_ nestedMaybeX: Float??) -> Float {
    switch nestedMaybeX {
    case nil: return 10
    case let .some(maybeX):
      switch maybeX {
        case nil: return 10
        case let .some(x): return x * x
      }
    }
  }
  expectEqual(gradient(at: 10, in: optional_nested3), .init(.init(20.0)))
  expectEqual(gradient(at: nil, in: optional_nested3), .init(.init(0.0)))
}

OptionalTests.test("Var1") {
  @differentiable
  func optional4(_ maybeX: Float?) -> Float {
    var maybeX = maybeX
    if let x = maybeX {
        return x * x
    }
    return 10
  }

  expectEqual(gradient(at: 10, in: optional4), .init(20.0))
  expectEqual(gradient(at: nil, in: optional4), .init(0.0))
}

OptionalTests.test("Var2") {
  @differentiable
  func optional5(_ maybeX: Float?) -> Float {
    if var x = maybeX {
      return x * x
    }
    return 10
  }

  expectEqual(gradient(at: 10, in: optional5), .init(20.0))
  expectEqual(gradient(at: nil, in: optional5), .init(0.0))
}

OptionalTests.test("Generic") {
  @differentiable
  func optional6<T: Differentiable>(_ maybeX: T?, _ defaultValue: T) -> T {
    switch maybeX {
    case nil: return defaultValue
    case let .some(x): return x
    }
  }

  expectEqual(gradient(at: 10, 20, in: optional6), (.init(1.0), 0.0))
  expectEqual(gradient(at: nil, 20, in: optional6), (.init(0.0), 1.0))
}

runAllTests()
