// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=upcoming-swift)

// REQUIRES: executable_test

import StdlibUnittest
import ImportAsMember

var NamespacesTestSuite = TestSuite("Import as member of namespace")

NamespacesTestSuite.test("Struct in a namespace") {
  let s = MyNS.NestedStruct()
  expectEqual(123, s.method())
  expectEqual(124, s.methodConstRef())
  expectEqual(125, s.methodInline())
}

NamespacesTestSuite.test("Struct in a deep namespace") {
  let s = MyNS.MyDeepNS.DeepNestedStruct()
  expectEqual(456, s.method())
  expectEqual(458, s.methodConstRef())
  expectEqual(459, s.methodTypedef())
  expectEqual(460, s.methodTypedefQualName())
}

NamespacesTestSuite.test("Struct and function in a namespace") {
  let s = MyNS.NestedStruct()
  expectEqual(126, s.nestedMethod())
  expectEqual(127, s.nestedMethodInline())
}

runAllTests()
