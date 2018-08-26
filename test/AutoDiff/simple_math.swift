// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var SimpleMathTests = TestSuite("SimpleMath")

struct Vector : Equatable {
  var x: Float
  var y: Float

  init(_ x: Float, _ y: Float) { self.x = x; self.y = y }
  @differentiable(reverse, adjoint: dAdd)
  static func + (lhs: Vector, rhs: Vector) -> Vector {
    return Vector(lhs.x + rhs.x, lhs.y + rhs.y)
  }
  static func dAdd(lhs: Vector, rhs: Vector, _: Vector, seed: Vector) -> (Vector, Vector) {
    return (seed, seed)
  }
}

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

SimpleMathTests.test("Aggregates") {
  let grad0 = #gradient({ (x: Vector, y: Vector) -> Vector in
    return x
  })
  expectEqual((Vector(1, 1), Vector(0, 0)),
              grad0(Vector(1, 2), Vector(10, 20)))

  let grad1 = #gradient({ (x: Vector, y: Vector) -> Vector in
    return x + y
  })
  expectEqual((Vector(1, 1), Vector(0, 0)),
              grad1(Vector(1, 2), Vector(10, 20)))
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
}

runAllTests()
