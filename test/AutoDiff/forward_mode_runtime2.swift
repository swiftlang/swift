// RUN: %target_run_simple_swift_forward_mode_differentiation
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var ForwardModeTests = TestSuite("ForwardMode")

ForwardModeTests.test("GenericTrackedBinaryLets") {
  func foo<T>(_ x: Tracked<T>, _ y: Tracked<T>) -> Tracked<T>
    where T: Differentiable & SignedNumeric,
          T == T.TangentVector,
          T == T.Magnitude {
    let a = x * y // xy
    let b = a + a // 2xy
    return b + b // 4xy
  }
  // 4y + 4x
  let (y, differential) = valueWithDifferential(at: 4, 5) { (x: Float, y: Float) in
    foo(Tracked(x), Tracked(y))
  }
  expectEqual(80, y)
  expectEqual(36, differential(1, 1))
}

ForwardModeTests.test("GenericTrackedBinaryVars") {
  func foo<T>(_ x: Tracked<T>, _ y: Tracked<T>) -> Tracked<T>
    where T: Differentiable & SignedNumeric,
          T == T.TangentVector,
          T == T.Magnitude {
    var a = x * y // xy
    a = a + a // 2xy
    var b = x
    b = a
    return b + b // 4xy
  }
  // 4y + 4x
  let (y, differential) = valueWithDifferential(at: 4, 5) { (x: Float, y: Float) in
    foo(Tracked(x), Tracked(y))
  }
  expectEqual(80, y)
  expectEqual(36, differential(1, 1))
}

ForwardModeTests.test("TrackedWithLets") {
  func foo(x: Tracked<Float>, y: Tracked<Float>) -> Tracked<Float> {
    let a = x + y
    let b = a * a // (x+y)^2
    let c = b / x + y // (x+y)^2/x+y
    return c
  }
  // (3x^2+2xy-y^2)/x^2+1
  let (y, differential) = valueWithDifferential(at: 4, 5, in: foo)
  expectEqual(25.25, y)
  expectEqual(4.9375, differential(1, 1))
}

runAllTests()
