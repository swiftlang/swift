// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// REQUIRES: executable_test

import StdlibUnittest
import ConstexprInNamespace

var NamespacesTestSuite = TestSuite("Constexpr in namespace")

NamespacesTestSuite.test("constexpr int") {
  expectEqual(NS1.myConstexprInt, 123)
}

NamespacesTestSuite.test("static constexpr int") {
  expectEqual(NS1.myStaticConstexprInt, 456)
}

runAllTests()
