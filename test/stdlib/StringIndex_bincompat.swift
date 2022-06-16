// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g %s -o %t/StringIndex
// RUN: %target-codesign %t/StringIndex
// RUN: env %env-SWIFT_BINARY_COMPATIBILITY_VERSION=0x050600 %target-run %t/StringIndex %S/Inputs/

// REQUIRES: executable_test
// UNSUPPORTED: freestanding
// UNSUPPORTED: use_os_stdlib || back_deployment_runtime
// UNSUPPORTED: swift_stdlib_asserts

// In 5.7, a number of String APIs started performing stronger index validation,
// eliminating issues such as out of bounds memory accesses when members are
// given invalid indices. The environment variable
// SWIFT_BINARY_COMPATIBILITY_VERSION above forces the stdlib's bincompat
// version to 5.6 so that we can test older behavior.
//
// We can only test old behavior that doesn't lead to undefined behavior,
// though.

import StdlibUnittest

var suite = TestSuite("StringIndexBinCompatTests")
defer { runAllTests() }

suite.test("String.index(before:) on an index near the start") {
  let string = "😲 hi"
  let i = string.utf8.index(after: string.utf8.startIndex)
  expectEqual(string.index(before: i), string.startIndex)
}
