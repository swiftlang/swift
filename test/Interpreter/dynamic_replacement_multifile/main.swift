// RUN: %empty-directory(%t)
// RUN: %target-build-swift -o %t/main %s %S/Inputs/dynamic_replacement_multi_file_A.swift %S/Inputs/dynamic_replacement_multi_file_B.swift -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test

// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size

import StdlibUnittest

dynamic func replaceable() -> Int {
  return 0
}

dynamic func replaceableInOtherFile() -> Int {
  return 0
}

@_dynamicReplacement(for: replaceable())
func replaceable_r() -> Int {
  return 1
}

protocol P {}

extension Int : P {}

struct Pair {
  var x: Int64 = 0
  var y: Int64 = 0
}

extension Pair : P {}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
dynamic func bar(_ x: Int) -> some P {
  return x
}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
@_dynamicReplacement(for: bar(_:))
func bar_r(_ x: Int) -> some P {
  return Pair()
}

var DynamicallyReplaceable = TestSuite("DynamicallyReplaceable")

DynamicallyReplaceable.test("DynamicallyReplaceable") {
  if #available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *) {
    expectEqual(1, replaceable())
    expectEqual(2, replaceable1())
    expectEqual(3, replaceable2())
    expectEqual(7, replaceableInOtherFile())
    expectEqual(16, MemoryLayout.size(ofValue: bar(5)))
    expectEqual(16, MemoryLayout.size(ofValue: bar1(5)))
    expectEqual(16, MemoryLayout.size(ofValue: bar2(5)))
    expectEqual(16, MemoryLayout.size(ofValue: bar3(5)))
  }

}

runAllTests()
