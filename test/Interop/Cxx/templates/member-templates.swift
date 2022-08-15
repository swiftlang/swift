// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

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
  expectEqual(h.addSameTypeParams(2, 1), 3)
  expectEqual(h.addMixedTypeParams(2, 1), 3)
}

// TODO: support this rdar://88443730
// TemplatesTestSuite.test("Returns other specialization") {
//   let t = TemplateClassWithMemberTemplates<CInt>(42)
//   var _5 = 5
//   let o = t.toOtherSpec(&_5)
// }

runAllTests()
