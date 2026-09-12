// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking)

// REQUIRES: executable_test

import FunctionTemplateWithOptionalFrt
import StdlibUnittest

var FunctionTemplateWithOptionalFrtTestSuite =
    TestSuite("Function templates with optional foreign reference types")

FunctionTemplateWithOptionalFrtTestSuite.test("downcast succeeds") {
  let derived: FRTDerived? = downcast(makeDerivedAsBase())
  expectNotNil(derived)
  expectEqual(2, derived!.y)
}

FunctionTemplateWithOptionalFrtTestSuite.test("downcast fails") {
  let derived: FRTDerived? = downcast(makeBase())
  expectNil(derived)
}

FunctionTemplateWithOptionalFrtTestSuite.test("nullableDowncast succeeds") {
  let derived: FRTDerived? = nullableDowncast(makeDerivedAsBase())
  expectNotNil(derived)
  expectEqual(2, derived!.y)
  expectEqual(1, derived!.x)
}

FunctionTemplateWithOptionalFrtTestSuite.test("nullableDowncast fails") {
  let derived: FRTDerived? = nullableDowncast(makeBase())
  expectNil(derived)
  let optionalBase: FRTBase? = makeBase()
  let alsoDerived: FRTDerived? = nullableDowncast(optionalBase)
  expectNil(alsoDerived)
}

runAllTests()
