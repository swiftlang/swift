// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-run %t/a.out4

// REQUIRES: executable_test

import StdlibUnittest

var tests = TestSuite("RangeReplaceableFilterCompatibility")

tests.test("String.filter return type") {
  var filtered = "Hello, World".filter { $0 < "A" }
  expectType(String.self, &filtered)
}

tests.test("Array.filter return type") {
  var filtered = Array(0..<10).filter { $0 % 2 == 0 }
  expectType([Int].self, &filtered)
}

tests.test("ContiguousArray.filter return type") {
  var filtered = ContiguousArray(0..<10).filter { $0 % 2 == 0 }
  expectType([Int].self, &filtered)
}

tests.test("ArraySlice.filter return type") {
  var filtered = Array(0..<10)[0..<10].filter { $0 % 2 == 0 }
  expectType([Int].self, &filtered)
}

tests.test("String.filter can return [Character]") {
  let filtered = "Hello, World".filter { "A" <= $0 && $0 <= "Z"} as [Character]
  expectEqualSequence("HW", filtered)
}

tests.test("lazy.flatMap.filter ambiguity") {
  // this expression should compile without ambiguity
  _ = Array(0..<10).lazy.flatMap { .some($0) }.filter { _ in false }
}

runAllTests()

