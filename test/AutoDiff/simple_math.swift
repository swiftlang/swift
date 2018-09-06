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
    x + x + x * y * sin(1.0)
  })
  expectEqual((3.682942, 2.5244129), dfoo3(3, 2))
}

SimpleMathTests.test("FunctionCall") {
  func foo(_ x: Float, _ y: Float) -> Float {
    return x * 3 + bar(3) * y
  }
  func bar(_ x: Float) -> Float {
    return 3 * x
  }
  let dfoo = #gradient(foo)
  expectEqual((3, 9), dfoo(3, 4))
  let dfoo_dx = #gradient(foo, wrt: .0)
  expectEqual(3, dfoo_dx(3, 4))
}

runAllTests()
