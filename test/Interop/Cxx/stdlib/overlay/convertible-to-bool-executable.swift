// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking)
// REQUIRES: executable_test

import StdlibUnittest
import ConvertibleToBool

var CxxConvertibleToBoolTestSuite = TestSuite("CxxConvertibleToBool")

CxxConvertibleToBoolTestSuite.test("BoolBox as CxxConvertibleToBool") {
  let b1 = BoolBox(value: true)
  expectTrue(Bool(fromCxx: b1))

  let b2 = BoolBox(value: false)
  expectFalse(Bool(fromCxx: b2))
}

CxxConvertibleToBoolTestSuite.test("DualOverloadBoolBox as CxxConvertibleToBool") {
  let b1 = DualOverloadBoolBox(value: true)
  expectTrue(Bool(fromCxx: b1))

  let b2 = DualOverloadBoolBox(value: false)
  expectFalse(Bool(fromCxx: b2))
}

CxxConvertibleToBoolTestSuite.test("ExplicitBoolBox as CxxConvertibleToBool") {
  // value is private; just exercise the conversion path.
  let _ = Bool(fromCxx: ExplicitBoolBox())
}

CxxConvertibleToBoolTestSuite.test("BoolBoxWithOtherConversions as CxxConvertibleToBool") {
  let b1 = BoolBoxWithOtherConversions(value: 1)
  expectTrue(Bool(fromCxx: b1))

  let b2 = BoolBoxWithOtherConversions(value: 0)
  expectFalse(Bool(fromCxx: b2))
}

CxxConvertibleToBoolTestSuite.test("InheritedBoolBox as CxxConvertibleToBool") {
  var b = InheritedBoolBox()
  b.set(false)
  expectFalse(Bool(fromCxx: b))
  b.set(true)
  expectTrue(Bool(fromCxx: b))
}

CxxConvertibleToBoolTestSuite.test("OverriddenBoolBox as CxxConvertibleToBool") {
  // Note: OverriddenBoolBox::operator bool() returns !value
  var b = OverriddenBoolBox()
  b.set(false)
  expectTrue(Bool(fromCxx: b))
  b.set(true)
  expectFalse(Bool(fromCxx: b))
}

CxxConvertibleToBoolTestSuite.test("VirtualDiamondBoolBox as CxxConvertibleToBool") {
  // Virtual diamond inheritance — single shared BoolBox subobject.
  var b = VirtualDiamondBoolBox()
  b.set(true)
  expectTrue(Bool(fromCxx: b))
  b.set(false)
  expectFalse(Bool(fromCxx: b))
}

CxxConvertibleToBoolTestSuite.test("PublicUsingBoolBox as CxxConvertibleToBool") {
  // ProtectedBoolBox::operator bool always returns true; using-decl makes it public.
  expectTrue(Bool(fromCxx: PublicUsingBoolBox()))
}

runAllTests()
