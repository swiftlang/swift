// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=default)

// REQUIRES: executable_test

import StdlibUnittest
import NestedEnums

var EnumsTestSuite = TestSuite("Nested Enums")

EnumsTestSuite.test("scoped enums nested in a struct") {
  let s1 = Struct.Scoped.S1
  let s2 = Struct.Scoped.S2
  let s3 = Struct.Scoped.S3
  expectNotEqual(s1, s2)
  expectNotEqual(s2, s3)
  expectNotEqual(s1, s3)
}

EnumsTestSuite.test("unscoped enums nested in a struct") {
  let u1 = Struct.U1
  let u2 = Struct.U2
  let u3 = Struct.U3
  expectNotEqual(u1, u2)
  expectNotEqual(u2, u3)
  expectNotEqual(u1, u3)
}

EnumsTestSuite.test("anonymous enums nested in a struct") {
  let a1 = Struct.A1
  let a2 = Struct.A2
  let a3 = Struct.A3
  expectNotEqual(a1, a2)
  expectNotEqual(a2, a3)
  expectNotEqual(a1, a3)
}

EnumsTestSuite.test("scoped enums nested in a namespace") {
  let s1 = Namespace.Scoped.S1
  let s2 = Namespace.Scoped.S2
  let s3 = Namespace.Scoped.S3
  expectNotEqual(s1, s2)
  expectNotEqual(s2, s3)
  expectNotEqual(s1, s3)
}

EnumsTestSuite.test("unscoped enums nested in a namespace") {
  let u1 = Namespace.U1
  let u2 = Namespace.U2
  let u3 = Namespace.U3
  expectNotEqual(u1, u2)
  expectNotEqual(u2, u3)
  expectNotEqual(u1, u3)
}

EnumsTestSuite.test("anonymous enums in a namespace") {
  let a1 = Namespace.A1
  let a2 = Namespace.A2
  let a3 = Namespace.A3
  expectNotEqual(a1, a2)
  expectNotEqual(a2, a3)
  expectNotEqual(a1, a3)
}

runAllTests()
