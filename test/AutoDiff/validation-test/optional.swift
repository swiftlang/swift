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
  func optional_let(_ maybeX: Float?) -> Float {
    if let x = maybeX {
        return x * x
    }
    return 10
  }
  expectEqual(gradient(at: 10, in: optional_let), .init(20.0))
  expectEqual(gradient(at: nil, in: optional_let), .init(0.0))

  @differentiable
  func optional_let_nested(_ nestedMaybeX: Float??) -> Float {
    if let maybeX = nestedMaybeX {
      if let x = maybeX {
        return x * x
      }
      return 10
    }
    return 10
  }
  expectEqual(gradient(at: 10, in: optional_let_nested), .init(.init(20.0)))
  expectEqual(gradient(at: nil, in: optional_let_nested), .init(.init(0.0)))

  @differentiable
  func optional_let_generic<T: Differentiable>(_ maybeX: T?, _ defaultValue: T) -> T {
    if let x = maybeX {
        return x
    }
    return defaultValue
  }

  expectEqual(gradient(at: 10, 20, in: optional_let_generic), (.init(1.0), 0.0))
  expectEqual(gradient(at: nil, 20, in: optional_let_generic), (.init(0.0), 1.0))

  // This test is failing
  /* @differentiable
  func optional_let_nested_generic<T: Differentiable>(_ nestedMaybeX: T??, _ defaultValue: T) -> T {
    if let maybeX = nestedMaybeX {
      if let x = maybeX {
        return x
      }
      return defaultValue
    }
    return defaultValue
  }

  expectEqual(gradient(at: 10, 20, in: optional_let_nested_generic), (.init(1.0), 0.0))
  expectEqual(gradient(at: nil, 20, in: optional_let_nested_generic), (.init(0.0), 1.0)) */
}

OptionalTests.test("Switch") {
  @differentiable
  func optional_switch(_ maybeX: Float?) -> Float {
    switch maybeX {
    case nil: return 10
    case let .some(x): return x * x
    }
  }

  expectEqual(gradient(at: 10, in: optional_switch), .init(20.0))
  expectEqual(gradient(at: nil, in: optional_switch), .init(0.0))

  @differentiable
  func optional_switch_nested(_ nestedMaybeX: Float??) -> Float {
    switch nestedMaybeX {
    case nil: return 10
    case let .some(maybeX):
      switch maybeX {
        case nil: return 10
        case let .some(x): return x * x
      }
    }
  }

  expectEqual(gradient(at: 10, in: optional_switch_nested), .init(.init(20.0)))
  expectEqual(gradient(at: nil, in: optional_switch_nested), .init(.init(0.0)))

  @differentiable
  func optional_switch_generic<T: Differentiable>(_ maybeX: T?, _ defaultValue: T) -> T {
    switch maybeX {
    case nil: return defaultValue
    case let .some(x): return x
    }
  }

  expectEqual(gradient(at: 10, 20, in: optional_switch_generic), (.init(1.0), 0.0))
  expectEqual(gradient(at: nil, 20, in: optional_switch_generic), (.init(0.0), 1.0))

  // TODO: this test is failing
  /* @differentiable
  func optional_switch_nested_generic<T: Differentiable>(_ nestedMaybeX: T??, _ defaultValue: T) -> T {
    switch nestedMaybeX {
    case nil: return defaultValue
    case let .some(maybeX):
      switch maybeX {
        case nil: return defaultValue
        case let .some(x): return x
      }
    }
  }

  expectEqual(gradient(at: 10, 20, in: optional_switch_nested_generic), (.init(1.0), 0.0))
  expectEqual(gradient(at: nil, 20, in: optional_switch_nested_generic), (.init(0.0), 1.0)) */
}

OptionalTests.test("Var1") {
  @differentiable
  func optional_var1(_ maybeX: Float?) -> Float {
    var maybeX = maybeX
    if let x = maybeX {
        return x * x
    }
    return 10
  }

  expectEqual(gradient(at: 10, in: optional_var1), .init(20.0))
  expectEqual(gradient(at: nil, in: optional_var1), .init(0.0))

  @differentiable
  func optional_var1_generic<T: Differentiable>(_ maybeX: T?, _ defaultValue: T) -> T {
    var maybeX = maybeX
    if let x = maybeX {
        return x
    }
    return defaultValue
  }

  expectEqual(gradient(at: 10, 20, in: optional_var1_generic), (.init(1.0), 0.0))
  expectEqual(gradient(at: nil, 20, in: optional_var1_generic), (.init(0.0), 1.0))
}

OptionalTests.test("Var2") {
  @differentiable
  func optional_var2(_ maybeX: Float?) -> Float {
    if var x = maybeX {
      return x * x
    }
    return 10
  }

  expectEqual(gradient(at: 10, in: optional_var2), .init(20.0))
  expectEqual(gradient(at: nil, in: optional_var2), .init(0.0))

  @differentiable
  func optional_var2_generic<T: Differentiable>(_ maybeX: T?, _ defaultValue: T) -> T {
    if var x = maybeX {
      return x
    }
    return defaultValue
  }

  expectEqual(gradient(at: 10, 20, in: optional_var2_generic), (.init(1.0), 0.0))
  expectEqual(gradient(at: nil, 20, in: optional_var2_generic), (.init(0.0), 1.0))
}

runAllTests()
