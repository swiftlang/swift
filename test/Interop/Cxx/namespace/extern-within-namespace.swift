// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=upcoming-swift)

// REQUIRES: executable_test

import StdlibUnittest
import ExternWithinNamespace

var ExternTestSuite = TestSuite("Extern block within namespaces")

ExternTestSuite.test("Function within extern block within namespace") {
  let r = Outer.Inner.foobar()
  expectEqual(r, 123)
}

ExternTestSuite.test("Function within extern block within inline namespace") {
  let r = Outer.InnerInline.baz()
  expectEqual(r, 321)
}

ExternTestSuite.test("Function within extern block within extern block") {
  let r = ExternWithinExtern.Inner.deep()
  expectEqual(r, 42)
}

runAllTests()
