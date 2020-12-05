// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc
// REQUIRES: fixing-after-30630

import MemberTemplates
import StdlibUnittest

var TemplatesTestSuite = TestSuite("Member Templates")

TemplatesTestSuite.test("Set value - IntWrapper") {
  var w = IntWrapper(11)
  w.setValue(42)
  expectEqual(w.value, 42)
}

TemplatesTestSuite.test("Templated Add") {
  var h = HasMemberTemplates()
  expectEqual(h.add(2, 1), 3)
  expectEqual(h.addTwoTemplates(2, 1), 3)
}

runAllTests()
