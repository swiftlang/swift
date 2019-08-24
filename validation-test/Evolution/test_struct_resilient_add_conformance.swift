// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_resilient_add_conformance


var StructResilientAddConformanceTest = TestSuite("StructResilientAddConformance")

StructResilientAddConformanceTest.test("AddConformance") {
  var t = AddConformance()

  do {
    t.x = 10
    t.y = 20
    expectEqual(t.x, 10)
    expectEqual(t.y, 20)
  }

  if (getVersion() == 0) {
    expectEqual(workWithPointLike(t), 0)
  } else {
    expectEqual(workWithPointLike(t), 200)
  }
}

#if AFTER
protocol MyPointLike {
  var x: Int { get set }
  var y: Int { get set }
}

protocol MyPoint3DLike {
  var z: Int { get set }
}

extension AddConformance : MyPointLike {}
extension AddConformance : MyPoint3DLike {}

@inline(never) func workWithMyPointLike<T>(_ t: T) {
  var p = t as! MyPointLike
  p.x = 50
  p.y = 60
  expectEqual(p.x, 50)
  expectEqual(p.y, 60)
}

StructResilientAddConformanceTest.test("MyPointLike") {
  var p: MyPointLike = AddConformance()

  do {
    p.x = 50
    p.y = 60
    expectEqual(p.x, 50)
    expectEqual(p.y, 60)
  }

  workWithMyPointLike(p)
}
#endif

runAllTests()

