// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_static_stored_to_computed.swift -o %t/before/struct_static_stored_to_computed.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_static_stored_to_computed.swift -o %t/before/struct_static_stored_to_computed.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_static_stored_to_computed.swift -o %t/after/struct_static_stored_to_computed.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_static_stored_to_computed.swift -o %t/after/struct_static_stored_to_computed.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/struct_static_stored_to_computed.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/struct_static_stored_to_computed.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/struct_static_stored_to_computed.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/struct_static_stored_to_computed.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

import StdlibUnittest
import struct_static_stored_to_computed

var StructStaticChangeStoredToComputedTest = TestSuite("StructStaticChangeStoredToComputed")

StructStaticChangeStoredToComputedTest.test("ChangeStoredToComputed") {
  do {
    @inline(never) func twice(inout x: Int) {
      x *= 2
    }

    expectEqual(ChangeStoredToComputed.value, 0)
    ChangeStoredToComputed.value = 32
    expectEqual(ChangeStoredToComputed.value, 32)
    ChangeStoredToComputed.value = -128
    expectEqual(ChangeStoredToComputed.value, -128)
    twice(&ChangeStoredToComputed.value)
    expectEqual(ChangeStoredToComputed.value, -256)
  }
}

runAllTests()
