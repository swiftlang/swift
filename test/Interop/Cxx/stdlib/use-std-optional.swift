// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import StdlibUnittest
import StdOptional
import CxxStdlib

var StdOptionalTestSuite = TestSuite("StdOptional")

StdOptionalTestSuite.test("pointee") {
  let nonNilOpt = getNonNilOptional()
  let pointee = nonNilOpt.pointee
  expectEqual(123, pointee)
}

runAllTests()
