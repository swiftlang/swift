// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
// REQUIRES: executable_test

import StdlibUnittest
import StdEnableSharedFromThis

var StdESFT = TestSuite("StdEnableSharedFromThis")

StdESFT.test("User type that inherits from std::enable_shared_from_this") {
  let s = SharableFromThis()
  expectEqual(s.field, 42)
}

runAllTests()
