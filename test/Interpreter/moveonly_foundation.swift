// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: foundation

import StdlibUnittest
import Foundation

defer { runAllTests() }

var Tests = TestSuite("MoveOnlyFoundationTests")

// coverage for rdar://132915515
// CHECK-LABEL: borrowing get on copyable type
Tests.test("borrowing get on copyable type") {
    struct Loaner {
      let value: UUID
      var property: UUID { borrowing get { value }}
      borrowing func method() -> UUID { value }
    }

    func printLoanedValue(_ loaner: borrowing Loaner) {
      // CHECK: UUID = [[UUID_VAL:.*]]
      print("UUID = \(loaner.property)")

      // CHECK: UUID = [[UUID_VAL]]
      print("UUID = \(loaner.method())")
    }

    do {
      let l = Loaner(value: UUID())
      printLoanedValue(l)
    }
}
