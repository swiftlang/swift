// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import Constructors

var CxxConstructorTestSuite = TestSuite("CxxConstructors")

CxxConstructorTestSuite.test("ExplicitDefaultConstructor") {
  let instance = ExplicitDefaultConstructor()

  expectEqual(42, instance.x)
}

CxxConstructorTestSuite.test("ImplicitDefaultConstructor") {
  let instance = ImplicitDefaultConstructor()

  expectEqual(42, instance.x)
}

CxxConstructorTestSuite.test("DefaultedDefaultConstructor") {
  let instance = DefaultedDefaultConstructor()

  expectEqual(42, instance.x)
}

CxxConstructorTestSuite.test("MemberOfClassType") {
  let instance = MemberOfClassType()

  expectEqual(42, instance.member.x)
}

CxxConstructorTestSuite.test("ConstructorWithParam") {
  let instance = ConstructorWithParam(2)

  expectEqual(44, instance.x)
}

CxxConstructorTestSuite.test("TemplatedConstructor") {
  let arg = ArgType(i: 2)
  let instance = TemplatedConstructor(arg)

  expectEqual(2, instance.value.i)
}

CxxConstructorTestSuite.test("implicit default ctor") {
  // Make sure that fields of C++ structs are zeroed out.

  let instance1 = ConstructorWithParam()
  expectEqual(0, instance1.x)

  let instance2 = IntWrapper()
  expectEqual(0, instance2.x)

  // CopyAndMoveConstructor is not default-initializable in C++, however, Swift
  // generates an implicit deprecated default constructor for C++ structs for
  // compatibility with C. This constructor will zero out the entire backing
  // memory of the struct, including fields that have an init expression.
  // See `SwiftDeclSynthesizer::createDefaultConstructor`.
  let instance3 = CopyAndMoveConstructor()
  expectEqual(0, instance3.value)
  expectNil(instance3.ptr)
}

runAllTests()
