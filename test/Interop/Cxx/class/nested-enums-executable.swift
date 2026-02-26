// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=default

import StdlibUnittest
import NestedEnums

var EnumsTestSuite = TestSuite("Nested Enums")

EnumsTestSuite.test("scoped enums") {
  let s1 = HasEnums.Scoped.S1
  let s2 = HasEnums.Scoped.S2
  let s3 = HasEnums.Scoped.S3
  expectNotEqual(s1, s2)
  expectNotEqual(s2, s3)
  expectNotEqual(s1, s3)
}

EnumsTestSuite.test("unscoped enums") {
  let u1 = HasEnums.U1
  let u2 = HasEnums.U2
  let u3 = HasEnums.U3
  expectNotEqual(u1, u2)
  expectNotEqual(u2, u3)
  expectNotEqual(u1, u3)
}

EnumsTestSuite.test("anonymous enums") {
  let a1 = HasEnums.A1
  let a2 = HasEnums.A2
  let a3 = HasEnums.A3
  expectNotEqual(a1, a2)
  expectNotEqual(a2, a3)
  expectNotEqual(a1, a3)
}

runAllTests()
