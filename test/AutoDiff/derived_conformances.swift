// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var DerivedConformanceTests = TestSuite("DerivedConformances")

DerivedConformanceTests.test("MemberwiseInitializers") {
  struct AllVarStoredPropertiesHaveInitialValue: Differentiable, AdditiveArithmetic {
    var x = Float(100)
    var y = Float(100)
  }
  // Verify that `.zero` actually initializes properties to zero.
  expectEqual(AllVarStoredPropertiesHaveInitialValue(x: 0, y: 0),
              AllVarStoredPropertiesHaveInitialValue.zero)
  expectEqual(AllVarStoredPropertiesHaveInitialValue.zero.x, 0)
  expectEqual(AllVarStoredPropertiesHaveInitialValue.zero.y, 0)

  struct HasNoDerivativeConstant: Differentiable {
    @noDerivative let constant1 = Float(1)
    @noDerivative let constant2 = Double(1)
    var x = Float(1)
  }
  expectEqual(HasNoDerivativeConstant.AllDifferentiableVariables(x: 0),
              HasNoDerivativeConstant.AllDifferentiableVariables.zero)
  expectEqual(HasNoDerivativeConstant.CotangentVector(x: 0),
              HasNoDerivativeConstant.CotangentVector.zero)
}

runAllTests()
