// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs/ -o %t/cxx_interop -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/cxx_interop
// RUN: %target-run %t/cxx_interop
//
// REQUIRES: executable_test

import StdlibUnittest
import CxxConstructors

var CxxConstructorTestSuite = TestSuite("CxxConstructors")

CxxConstructorTestSuite.test("ExplicitDefaultConstructor") {
  let instance = ExplicitDefaultConstructor()

  expectEqual(42, instance.x)
}

CxxConstructorTestSuite.test("ImplicitDefaultConstructor") {
  let instance = ImplicitDefaultConstructor()

  expectEqual(42, instance.x)
}

CxxConstructorTestSuite.test("MemberOfClassType") {
  let instance = MemberOfClassType()

  expectEqual(42, instance.member.x)
}

CxxConstructorTestSuite.test("ConstructorWithParam") {
  let instance = ConstructorWithParam(123456)

  expectEqual(123456, instance.x)
}

runAllTests()
