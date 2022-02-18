// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
//
// REQUIRES: executable_test

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

DependentTypesTestSuite.test("Multiple arguments (inferred type).") {
  let m = multipleArgs(M<Int>(value: 40), 2, 10) as! M<Int>
  expectEqual(m.getValue(), 42)
}

DependentTypesTestSuite.test("Multiple dependent arguments (inferred type).") {
  let m = multipleDependentArgsInferred(M<Int>(value: 42), M<CInt>(value: 0), 1, CInt(2)) as! M<Int>
  expectEqual(m.getValue(), 42)
}

DependentTypesTestSuite.test("Multiple dependent arguments (not inferred).") {
  let m = multipleDependentArgs(M<Int>(value: 42), M<CInt>(value: 0), T: Int.self, U: Int.self) as! M<Int>
  expectEqual(m.getValue(), 42)
}

runAllTests()
