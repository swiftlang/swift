// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/class_change_stored_to_computed.swift -o %t/before/class_change_stored_to_computed.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/class_change_stored_to_computed.swift -o %t/before/class_change_stored_to_computed.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/class_change_stored_to_computed.swift -o %t/after/class_change_stored_to_computed.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/class_change_stored_to_computed.swift -o %t/after/class_change_stored_to_computed.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/class_change_stored_to_computed.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/class_change_stored_to_computed.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/class_change_stored_to_computed.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/class_change_stored_to_computed.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

// REQUIRES: executable_test

import StdlibUnittest
import class_change_stored_to_computed

var ChangeStoredToComputedTest = TestSuite("ChangeStoredToComputed")

ChangeStoredToComputedTest.test("ChangeStoredToComputed") {
  var t = ChangeStoredToComputed()

  do {
    expectEqual(t.celsius, 0)
    expectEqual(t.fahrenheit, 32)
  }

  do {
    t.celsius = 10
    expectEqual(t.celsius, 10)
    expectEqual(t.fahrenheit, 50)
  }

  do {
    func increaseTemperature(inout t: Int) {
      t += 10
    }

    increaseTemperature(&t.celsius)

    expectEqual(t.celsius, 20)
    expectEqual(t.fahrenheit, 68)
  }
}

runAllTests()
