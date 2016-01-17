// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/global_stored_to_computed.swift -o %t/before/global_stored_to_computed.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/global_stored_to_computed.swift -o %t/before/global_stored_to_computed.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/global_stored_to_computed.swift -o %t/after/global_stored_to_computed.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/global_stored_to_computed.swift -o %t/after/global_stored_to_computed.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/global_stored_to_computed.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/global_stored_to_computed.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/global_stored_to_computed.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/global_stored_to_computed.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

import StdlibUnittest
import global_stored_to_computed

var GlobalStoredToComputed = TestSuite("GlobalStoredToComputed")

GlobalStoredToComputed.test("ChangeStoredToComputed") {
  do {
    @inline(never) func increment(inout x: Int) {
      x += 1
    }

    expectEqual(globalStoredToComputed, 0)
    increment(&globalStoredToComputed)
    expectEqual(globalStoredToComputed, 1)
    globalStoredToComputed = 0xbadf00d
    expectEqual(globalStoredToComputed, 0xbadf00d)
  }
}

runAllTests()

