// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import StdlibUnittest
import Closure

var ClosureTestSuite = TestSuite("Closure")

ClosureTestSuite.test("ConvertToFunctionPointer") {
  cfunc2({N in})
}

ClosureTestSuite.test("Pass FRT to function pointer") {
  cppGo({N in })
}

ClosureTestSuite.test("AssignClosureToStructMemberConstRefNonTrivial") {
  var s = ConstRefNonTrivialFPStruct()
  s.fp = { nt in }
  s.callFp(NonTrivial())
  // Read back and call through the function pointer directly.
  let f = s.fp
  f(NonTrivial())
}

ClosureTestSuite.test("AssignClosureToStructMemberConstRefTrivial") {
  var s = ConstRefTrivialFPStruct()
  var t = Trivial()
  t.i = 42
  s.fp = { t in }
  s.callFp(t)
  let f = s.fp
  f(t)
}

ClosureTestSuite.test("AssignClosureToStructMemberNullableConstRef") {
  var s = NullableConstRefNonTrivialFPStruct()
  s.fp = { nt in }
  s.callFp(NonTrivial())
  let f = s.fp
  f?(NonTrivial())
}

ClosureTestSuite.test("LValueCopyConstRefNonTrivial") {
  var s1 = ConstRefNonTrivialFPStruct()
  var s2 = ConstRefNonTrivialFPStruct()
  s1.fp = { _ in }
  s2.fp = s1.fp
  s2.callFp(NonTrivial())
}

ClosureTestSuite.test("LValueCopyConstRefTrivial") {
  var s1 = ConstRefTrivialFPStruct()
  var s2 = ConstRefTrivialFPStruct()
  var t = Trivial()
  t.i = 42
  s1.fp = { _ in }
  s2.fp = s1.fp
  s2.callFp(t)
}

ClosureTestSuite.test("LValueCopyNullableConstRef") {
  var s1 = NullableConstRefNonTrivialFPStruct()
  var s2 = NullableConstRefNonTrivialFPStruct()
  s1.fp = { _ in }
  s2.fp = s1.fp
  s2.callFp(NonTrivial())
}

runAllTests()
