// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_resilient_remove_conformance

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var StructResilientRemoveConformanceTest = TestSuite("StructResilientRemoveConformance")

StructResilientRemoveConformanceTest.test("RemoveConformance") {
  var t = RemoveConformance()

  do {
    t.x = 10
    t.y = 20
    expectEqual(t.x, 10)
    expectEqual(t.y, 20)
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

extension RemoveConformance : MyPointLike {}
extension RemoveConformance : MyPoint3DLike {}

@inline(never) func workWithMyPointLike<T>(t: T) {
  var p = t as! MyPointLike
  p.x = 50
  p.y = 60
  expectEqual(p.x, 50)
  expectEqual(p.y, 60)
}

StructResilientRemoveConformanceTest.test("MyPointLike") {
  var p: MyPointLike = RemoveConformance()

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

