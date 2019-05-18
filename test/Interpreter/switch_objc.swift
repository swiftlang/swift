// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

var SwitchTestSuite = TestSuite("SwitchObjC")
defer { runAllTests() }

SwitchTestSuite.test("Resilient Type Tuple Initialization Construction") {
  // This test make sure that this works for URL specifically. There is a
  // separate artificial test case in switch_resilience that uses our own
  // resilient type.
  enum Enum {
  case first(url: URL, void: Void)
  }

  func getEnum() -> Enum {
    let url = URL(string: "http://foobar.com")!
    return .first(url: url, void: ())
  }
  func getBool() -> Bool { return false }

  switch getEnum() {
  case let .first(x, y) where getBool():
    print("first")
  case .first:
    print("second")
  }

  switch getEnum() {
  case let .first(value) where getBool():
    print("third")
  case .first:
    print("fourth")
  }
  print("done")
}
