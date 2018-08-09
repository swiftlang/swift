// RUN: %empty-directory(%t)
// RUN: %target-build-swift -I %S/../Inputs/clang-importer-sdk/platform/any/usr/include %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest


import ctypes

var UnionTestSuite = TestSuite("Unions")

UnionTestSuite.test("Simple") {
  var u = IntOrFloat()
  expectEqual(0, u.i)
  expectEqual(0.0, u.f)

  u.i = 444
  expectEqual(444, u.i)
  u.f = 555.0
  expectEqual(555.0, u.f)
  expectNotEqual(444, u.i)
}

UnionTestSuite.test("Initializer") {
  var u = IntOrFloat(i: 777)
  expectEqual(777, u.i)
  expectNotEqual(555.0, u.f)
}

UnionTestSuite.test("StructWithUnion") {
  var s = StructWithNamedUnion()
  expectEqual(0, s.a)
  expectEqual(0, s.b)
  expectEqual(0, s.intfloat.i)
  expectEqual(0, s.intfloat.f)

  s.a = 111
  s.b = 222
  s.intfloat = IntOrFloat(i: 333)
  expectEqual(111, s.a)
  expectEqual(222, s.b)
  expectEqual(333, s.intfloat.i)
  s.intfloat = IntOrFloat(f: 444.0)
  expectEqual(444.0, s.intfloat.f)
  expectNotEqual(333, s.intfloat.i)
}

UnionTestSuite.test("StructWithUnnamedUnion") {
  var s = StructWithUnnamedUnion()
  expectEqual(0, s.intfloat.i)
  expectEqual(0, s.intfloat.f)

  s.intfloat.i = 100
  expectEqual(100, s.intfloat.i)
  expectNotEqual(100.0, s.intfloat.f)

  s.intfloat.f = 1.0
  expectEqual(1.0, s.intfloat.f)
  expectNotEqual(100, s.intfloat.i)
}

runAllTests()
