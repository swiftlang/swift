// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var SimpleMathTests = TestSuite("SimpleMath")

SimpleMathTests.test("Arithmetics") {
  let dfoo1 = #gradient({ (x: Float, y: Float) -> Float in
    return x * y
  })
  expectEqual((4, 3), dfoo1(3, 4))
  let dfoo2 = #gradient({ (x: Float, y: Float) -> Float in
    return -x * y
  })
  expectEqual((-4, -3), dfoo2(3, 4))
  let dfoo3 = #gradient({ (x: Float, y: Float) -> Float in
    return -x + y
  })
  expectEqual((-1, 1), dfoo3(3, 4))
}

SimpleMathTests.test("Fanout") {
  let dfoo1 = #gradient({ (x: Float) -> Float in
     x - x
  })
  expectEqual(0, dfoo1(100))
  let dfoo2 = #gradient({ (x: Float) -> Float in
     x + x
  })
  expectEqual(2, dfoo2(100))
  let dfoo3 = #gradient({ (x: Float, y: Float) -> Float in
    x + x + x * y
  })
  expectEqual((4, 3), dfoo3(3, 2))
}

SimpleMathTests.test("FunctionCall") {
  func foo(_ x: Float, _ y: Float) -> Float {
    return 3 * x + { $0 * 3 }(3) * y
  }
  expectEqual((3, 9), #gradient(foo)(3, 4))
  expectEqual(3, #gradient(foo, wrt: .0)(3, 4))
}

SimpleMathTests.test("ResultSelection") {
  func foo(_ x: Float, _ y: Float) -> (Float, Float) {
    return (x + 1, y + 2)
  }
  expectEqual((1, 0), #gradient(foo, result: .0)(3, 3))
  expectEqual((0, 1), #gradient(foo, result: .1)(3, 3))
}


SimpleMathTests.test("CaptureLocal") {
  let z: Float = 10
  func foo(_ x: Float) -> Float {
    return z * x
  }
  expectEqual(10, #gradient(foo)(0))
}

var globalVar: Float = 10
SimpleMathTests.test("CaptureGlobal") {
  let foo: (Float) -> Float = { x in
    globalVar += 20
    return globalVar * x
  }
  expectEqual(30, #gradient(foo)(0))
}

// FIXME: Forced inlining through @_transparent doesn't work on differential
// operators yet.
//
// SimpleMathTests.test("FunctionalDifferentialOperators") {
//   let x: Float = 3
//   let dydx = gradient(at: x) { x in
//     x * x
//   }
//   expectEqual(6, dydx)
// }

runAllTests()
