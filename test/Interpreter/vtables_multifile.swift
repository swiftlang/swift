// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(vtables_multifile_2)) -enable-library-evolution %S/Inputs/vtables_multifile_2.swift -emit-module -emit-module-path %t/vtables_multifile_2.swiftmodule
// RUN: %target-codesign %t/%target-library-name(vtables_multifile_2)

// RUN: %target-build-swift %s -L %t -I %t -lvtables_multifile_2 -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(vtables_multifile_2)

// REQUIRES: executable_test

import StdlibUnittest
import vtables_multifile_2

var VTableTestSuite = TestSuite("VTable")

open class OtherDerived : Derived {
  open override func privateMethod() -> Int {
    return super.privateMethod() + 1
  }
}

public final class OtherFinalDerived : Derived {
  public override func privateMethod() -> Int {
    return super.privateMethod() + 1
  }
}

VTableTestSuite.test("Base") {
  expectEqual(1, callBaseMethod(Base()))
}

VTableTestSuite.test("Derived") {
  expectEqual(2, callBaseMethod(Derived()))
  expectEqual(2, callDerivedMethod(Derived()))
}

VTableTestSuite.test("OtherDerived") {
  expectEqual(3, callBaseMethod(OtherDerived()))
  expectEqual(3, callDerivedMethod(OtherDerived()))
}

VTableTestSuite.test("OtherFinalDerived") {
  expectEqual(3, callBaseMethod(OtherFinalDerived()))
  expectEqual(3, callDerivedMethod(OtherFinalDerived()))
}

runAllTests()
