// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import MemberTemplates
import StdlibUnittest

var TemplatesTestSuite = TestSuite("Member Templates")

TemplatesTestSuite.test("Set value - IntWrapper") {
  var w = IntWrapper(11)

  expectEqual(w.value, 11)

  let v1: CInt = w.getValue()
  expectEqual(v1, 11)

  w.setValue(42)
  expectEqual(w.value, 42)
}

TemplatesTestSuite.test("IntHolder") {
  var h = IntHolder(11)

  let v1: CInt = h.getValue()
  expectEqual(v1, 11)

  let rv1: CInt = h.getValueRef().pointee
  expectEqual(rv1, 11)

  let r1: UnsafePointer<CInt> = h.getValueRef()
  expectEqual(r1.pointee, 11)

  h.value = 22

  let v2: CInt = h.getValue()
  expectEqual(v2, 22)

  let rv2: CInt = h.getValueRef().pointee
  expectEqual(rv2, 22)

  let r2: UnsafePointer<CInt> = h.getValueRef()
  expectEqual(r2.pointee, 22)

  expectEqual(r1.pointee, 22)
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
