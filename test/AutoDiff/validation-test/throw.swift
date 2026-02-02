// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ThrowingTests = TestSuite("Throwing")

enum E: Error {
  case error
}

ThrowingTests.testWithLeakChecking("SimpleTry") {
  @differentiable(reverse)
  func f(x: Double) throws -> Double {
    if x < 0 {
        throw E.error
    } else {
        return x * x
    }
  }

  expectEqual(4.0, gradient(at: 2.0, of: {x in try! f(x: x)}))
}

ThrowingTests.testWithLeakChecking("ActiveTry") {
  @differentiable(reverse)
  func f(x: Double) throws -> Double {
    if x < 0 {
        throw E.error
    } else {
        return x * x
    }
  }

  @differentiable(reverse)
  func g(x: Double) -> Double {
    do {
      return try f(x: x)
    } catch {
      return 2*x
    }
  }

  @differentiable(reverse)
  func h(x: Double) -> Double {
    let y = 5*x;
    do {
      let z = -x;
      return try f(x: z)
    } catch {
      return 2*y
    }
  }

  expectEqual(4.0, gradient(at: 2.0, of: g))
  expectEqual(2.0, gradient(at: -2.0, of: g))
  expectEqual(10.0, gradient(at: 2.0, of: h))
  expectEqual(-4.0, gradient(at: -2.0, of: h))
}

ThrowingTests.testWithLeakChecking("ActiveGenericTry") {
  @differentiable(reverse where T : Differentiable)
  func f<T>(x: T) throws -> T {
    return x
  }

  @differentiable(reverse where T : Differentiable)
  func g<T>(x: T) -> T {
    do {
      return try f(x: x)
    } catch {
      return x
    }
  }

  expectEqual(1.0, gradient(at: 2.0, of: g))
}


runAllTests()
