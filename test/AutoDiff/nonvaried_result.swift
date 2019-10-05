// RUN: %target-run-simple-swift
// TODO: Test forward-mode differentiation when it supports control flow.
// UN: %target_run_simple_swift_forward_mode_differentiation
// REQUIRES: executable_test

// Test differentiation edge case: functions with non-varied results.
// The differentials of these functions should return zero.
// The pullbacks of these functions should return zero with respect to the
// parameters for which the result is non-varying.

import StdlibUnittest
import DifferentiationUnittest

var NonVariedResultTests = TestSuite("TestCaseTests")

NonVariedResultTests.testWithLeakChecking("SingleBasicBlock") {
  @differentiable(wrt: y)
  func simple(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    return x
  }
  expectEqual(0, gradient(at: 3) { x in simple(10, x) })
  expectEqual((1, 0), gradient(at: 3, 4, in: simple))
}

NonVariedResultTests.testWithLeakChecking("Conditionals") {
  @differentiable(wrt: y)
  func `if`(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    if x > 0 {}
    return x
  }
  expectEqual(0, gradient(at: 3) { x in `if`(10, x) })
  expectEqual((1, 0), gradient(at: 3, 4, in: `if`))

  @differentiable(wrt: y)
  func `guard`(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    guard x > 0 else { return x }
    return x
  }
  expectEqual(0, gradient(at: 3) { x in `guard`(10, x) })
  expectEqual((1, 0), gradient(at: 3, 4, in: `guard`))

  @differentiable(wrt: y)
  func `switch`(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    switch x.value {
    case 0: break
    default: break
    }
    return x
  }
  expectEqual(0, gradient(at: 3) { x in `switch`(10, x) })
  expectEqual((1, 0), gradient(at: 3, 4, in: `switch`))
}

NonVariedResultTests.testWithLeakChecking("Loops") {
  @differentiable(wrt: y)
  func `for`(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    for i in 0..<10 {}
    return x
  }
  expectEqual(0, gradient(at: 3) { x in `for`(10, x) })
  expectEqual((1, 0), gradient(at: 3, 4, in: `for`))

  @differentiable(wrt: y)
  func `while`(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    while 0 < 0 {}
    return x
  }
  expectEqual(0, gradient(at: 3) { x in `while`(10, x) })
  expectEqual((1, 0), gradient(at: 3, 4, in: `while`))
}

NonVariedResultTests.testWithLeakChecking("Complex") {
  @differentiable(wrt: y)
  func complex(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    for i in 0..<10 {
      for j in 0..<10 {
        if x > 0 {}
        while 0 < 0 {}
        switch x.value {
        case 0: break
        default: break
        }
      }
    }
    return x + x + x
  }
  expectEqual(0, gradient(at: 3) { x in complex(10, x) })
  expectEqual((3, 0), gradient(at: 3, 4, in: complex))
}

runAllTests()
