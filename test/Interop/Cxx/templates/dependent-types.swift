// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
//
// REQUIRES: executable_test
//
// Failing on 32-bit platforms (rdar://89296327) (rdar://89166707)
// XFAIL: PTRSIZE=32

import DependentTypes
import StdlibUnittest

var DependentTypesTestSuite = TestSuite("DependentTypesTestSuite")

DependentTypesTestSuite.test("Different dependent arg and return type.") {
  let m1 = differentDependentArgAndRet(M<Int>(value: 42), T: Int.self, U: Int.self) as! M<Int>
  expectEqual(m1.getValue(), 42)

  let m2 = dependantReturnTypeSameAsArg(M<Int>(value: 42), T: Int.self) as! M<Int>
  expectEqual(m2.getValue(), 42)
}

DependentTypesTestSuite.test("Different dependent inferred by arg.") {
  let m = dependantReturnTypeInffered(42) as! M<Int>
  expectEqual(m.getValue(), 42)
}

DependentTypesTestSuite.test("Instantiate the same function twice") {
  // Intentionally test the same thing twice.
  let m = dependantReturnTypeInffered(42) as! M<Int>
  expectEqual(m.getValue(), 42)

  let m2 = dependantReturnTypeInffered(42) as! M<Int>
  expectEqual(m2.getValue(), 42)
}

DependentTypesTestSuite.test("Multiple arguments (inferred type).") {
  let m = multipleArgs(M<Int>(value: 40), 2, 10) as! M<Int>
  expectEqual(m.getValue(), 42)
}

DependentTypesTestSuite.test("Multiple dependent arguments (inferred type).") {
  let m = multipleDependentArgsInferred(M<Int>(value: 42), M<CInt>(value: 0), 1, CInt(2)) as! M<Int>
  expectEqual(m.getValue(), 42)
}

DependentTypesTestSuite.test("Multiple dependent arguments (not inferred).") {
  let m = multipleDependentArgs(M<Int>(value: 42), M<CInt>(value: 0), T: Int.self, U: CInt.self) as! M<Int>
  expectEqual(m.getValue(), 42)
}

DependentTypesTestSuite.test("Takes inout argument and returns dependent type.") {
  var x = 42
  let m = refToDependent(&x) as! M<Int>
  expectEqual(m.getValue(), 42)
}

DependentTypesTestSuite.test("Takes const ref and returns dependent type.") {
  let m = constRefToDependent(42) as! M<Int>
  expectEqual(m.getValue(), 42)
}


// We still have some problems calling methods on Windows: SR-13129 and rdar://88391102
#if !os(Windows)
DependentTypesTestSuite.test("Function template methods") {
  let m = M<Int>(value: 42)
  let m2 = m.memberDependentReturnType(CInt(32)) as! M<CInt>
  let val: Int = m2.memberDependentParamType(m)

  expectEqual(m2.getValue(), 32)
  expectEqual(val, 42)
}

DependentTypesTestSuite.test("Function template methods (mutable)") {
  var m = M<CInt>(value: 2)
  let m2 = m.memberDependentReturnTypeMutable(42) as! M<Int>
  expectEqual(m2.getValue(), 42)
}

DependentTypesTestSuite.test("Function template methods (static)") {
  let m = M<CInt>.memberDependentReturnTypeStatic(42) as! M<Int>
  expectEqual(m.getValue(), 42)

  let m2 = M<Int>.memberDependentReturnTypeStatic(CInt(32)) as! M<CInt>
  expectEqual(m2.getValue(), 32)
}
#endif // Windows

runAllTests()
