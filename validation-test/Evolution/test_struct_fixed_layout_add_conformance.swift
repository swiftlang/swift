// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_fixed_layout_add_conformance.swift -o %t/before/struct_fixed_layout_add_conformance.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_fixed_layout_add_conformance.swift -o %t/before/struct_fixed_layout_add_conformance.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_fixed_layout_add_conformance.swift -o %t/after/struct_fixed_layout_add_conformance.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_fixed_layout_add_conformance.swift -o %t/after/struct_fixed_layout_add_conformance.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/struct_fixed_layout_add_conformance.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/struct_fixed_layout_add_conformance.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/struct_fixed_layout_add_conformance.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/struct_fixed_layout_add_conformance.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

import StdlibUnittest
import struct_fixed_layout_add_conformance

var StructFixedLayoutAddConformanceTest = TestSuite("StructFixedLayoutAddConformance")

// FIXME: Once we have availability information for conformances, we can
// make this non-generic as long as we're careful to never directly
// reference an unavailable conformance table symbol
@inline(never) func workWithPointLike<T>(t: T) {
  if getVersion() > 0 {
    var p = t as! PointLike
    p.x = 30
    p.y = 40
    expectEqual(p.x, 30)
    expectEqual(p.y, 40)
  } else {
    expectEqual(t is PointLike, false)
  }
}

StructFixedLayoutAddConformanceTest.test("AddConformance") {
  var t = AddConformance()

  do {
    t.x = 10
    t.y = 20
    expectEqual(t.x, 10)
    expectEqual(t.y, 20)
  }

  workWithPointLike(t)
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

StructFixedLayoutAddConformanceTest.test("MyPointLike") {
  var p: MyPointLike = AddConformance()

  do {
    p.x = 50
    p.y = 60
    expectEqual(p.x, 50)
    expectEqual(p.y, 60)
  }
}
#endif

runAllTests()

