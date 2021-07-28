// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import MutableMembers

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("MutableMembers") {
  var obj = HasPublicMutableMember(a: 1)
  expectEqual(obj.foo(), 2)
  expectEqual(obj.foo(), 3)
}

runAllTests()
