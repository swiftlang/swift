// RUN: %target-run-simple-swift
// RUN: %target-run-simple-no-vjp-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var SimpleMathTests = TestSuite("SimpleMath")

SimpleMathTests.test("Arithmetics") {
  let foo1 = { (x: Float, y: Float) -> Float in
    return x * y
  }
  expectEqual((4, 3), gradient(at: 3, 4, in: foo1))
  let foo2 = { (x: Float, y: Float) -> Float in
    return -x * y
  }
  expectEqual((-4, -3), gradient(at: 3, 4, in: foo2))
  let foo3 = { (x: Float, y: Float) -> Float in
    return -x + y
  }
  expectEqual((-1, 1), gradient(at: 3, 4, in: foo3))
}

SimpleMathTests.test("Fanout") {
  let foo1 = { (x: Float) -> Float in
     x - x
  }
  expectEqual(0, gradient(at: 100, in: foo1))
  let foo2 = { (x: Float) -> Float in
     x + x
  }
  expectEqual(2, gradient(at: 100, in: foo2))
  let foo3 = { (x: Float, y: Float) -> Float in
    x + x + x * y
  }
  expectEqual((4, 3), gradient(at: 3, 2, in: foo3))
}

SimpleMathTests.test("FunctionCall") {
  func foo(_ x: Float, _ y: Float) -> Float {
    return 3 * x + { $0 * 3 }(3) * y
  }
  expectEqual((3, 9), gradient(at: 3, 4, in: foo))
  expectEqual(3, gradient(at: 3) { x in foo(x, 4) })
}

SimpleMathTests.test("ResultSelection") {
  func foo(_ x: Float, _ y: Float) -> (Float, Float) {
    return (x + 1, y + 2)
  }
  expectEqual((1, 0), gradient(at: 3, 3, in: { x, y in foo(x, y).0 }))
  expectEqual((0, 1), gradient(at: 3, 3, in: { x, y in foo(x, y).1 }))
}

SimpleMathTests.test("CaptureLocal") {
  let z: Float = 10
  func foo(_ x: Float) -> Float {
    return z * x
  }
  expectEqual(10, gradient(at: 0, in: foo))
}

var globalVar: Float = 10
SimpleMathTests.test("CaptureGlobal") {
  let foo: (Float) -> Float = { x in
    globalVar += 20
    return globalVar * x
  }
  expectEqual(30, gradient(at: 0, in: foo))
}

runAllTests()
