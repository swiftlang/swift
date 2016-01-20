// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_add_remove_conformances.swift -o %t/before/struct_add_remove_conformances.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_add_remove_conformances.swift -o %t/before/struct_add_remove_conformances.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_add_remove_conformances.swift -o %t/after/struct_add_remove_conformances.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_add_remove_conformances.swift -o %t/after/struct_add_remove_conformances.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/struct_add_remove_conformances.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/struct_add_remove_conformances.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/struct_add_remove_conformances.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/struct_add_remove_conformances.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

// Requires fixes to @_transparent attribute

import StdlibUnittest
import struct_add_remove_conformances

var StructAddRemoveConformancesTest = TestSuite("StructAddRemoveConformances")

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

StructAddRemoveConformancesTest.test("AddRemoveConformance") {
  var t = AddRemoveConformance()

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

extension AddRemoveConformance : MyPointLike {}
extension AddRemoveConformance : MyPoint3DLike {}

StructAddRemoveConformancesTest.test("MyPointLike") {
  var p: MyPointLike = AddRemoveConformance()

  p.x = 50
  p.y = 60
  expectEqual(p.x, 50)
  expectEqual(p.y, 60)
}
#endif

runAllTests()

