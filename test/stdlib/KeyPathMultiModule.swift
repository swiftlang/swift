// -- Test with resilience enabled
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -force-single-frontend-invocation -Xfrontend -enable-key-path-resilience -Xfrontend -enable-resilience -module-name KeyPathMultiModule_b -c -o %t/KeyPathMultiModule_b.o -emit-module-path %t/KeyPathMultiModule_b.swiftmodule -parse-as-library %S/Inputs/KeyPathMultiModule_b.swift
// RUN: %target-build-swift -g -Xfrontend -enable-key-path-resilience %t/KeyPathMultiModule_b.o -I %t %s -o %t/a.out.resilient
// RUN: %target-run %t/a.out.resilient

// -- Test again with resilience disabled, which changes the circumstances under
//    which we emit and use property descriptors
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -force-single-frontend-invocation -Xfrontend -enable-key-path-resilience -module-name KeyPathMultiModule_b -c -o %t/KeyPathMultiModule_b.o -emit-module-path %t/KeyPathMultiModule_b.swiftmodule -parse-as-library %S/Inputs/KeyPathMultiModule_b.swift
// RUN: %target-build-swift -Xfrontend -enable-key-path-resilience %t/KeyPathMultiModule_b.o -I %t %s -o %t/a.out.fragile
// RUN: %target-run %t/a.out.fragile

import KeyPathMultiModule_b
import StdlibUnittest

var keyPathMultiModule = TestSuite("key paths across multiple modules")

keyPathMultiModule.test("identity across multiple modules") {
  expectEqual(A_x_keypath(), \A.x)
  expectEqual(A_y_keypath(), \A.y)
  expectEqual(A_z_keypath(), \A.z)
  expectEqual(A_subscript_withGeneric_keypath(index: 0), \A.[withGeneric: 0])
  expectEqual(A_subscript_withGeneric_keypath(index: "butt"),
              \A.[withGeneric: "butt"])
  expectEqual(A_subscript_withGeneric_butt_keypath(),
              \A.[withGeneric: "pomeranian's big butt"])

  expectEqual(B_x_keypath(Double.self), \B<Double>.x)
  expectEqual(B_Int_x_keypath(), \B<Int>.x)
  expectEqual(B_y_keypath(Double.self), \B<Double>.y)
  expectEqual(B_Int_y_keypath(), \B<Int>.y)
  expectEqual(B_z_keypath(Double.self), \B<Double>.z)
  expectEqual(B_Int_z_keypath(), \B<Int>.z)
  expectEqual(B_subscript_withInt_keypath(Double.self, index: 1738),
              \B<Double>.[withInt: 1738])
  expectEqual(B_subscript_withInt_keypath(Double.self, index: 679),
              \B<Double>.[withInt: 679])
  expectEqual(B_Double_subscript_withInt_0_keypath(),
              \B<Double>.[withInt: 0])
  expectEqual(B_subscript_withGeneric_keypath(Double.self, index: "buttt"),
              \B<Double>.[withGeneric: "buttt"])
  expectEqual(B_Double_subscript_withGeneric_butt_keypath(),
              \B<Double>.[withGeneric: "Never is the universal butt type"])

  expectEqual(A_storedA_keypath(), \A.storedA)
  expectEqual(A_storedA_storedB_keypath(), \A.storedA.storedB)
  expectEqual(A_storedB_keypath(), \A.storedB)
  expectEqual(B_storedA_keypath(Double.self), \B<Double>.storedA)
  expectEqual(B_storedB_keypath(Double.self), \B<Double>.storedB)
  expectEqual(B_Int_storedA_keypath(), \B<Int>.storedA)
  expectEqual(B_Int_storedB_keypath(), \B<Int>.storedB)

  func testInGenericContext<X, Y: Hashable>(x: X, y: Y) {
    expectEqual(A_subscript_withGeneric_keypath(index: y), \A.[withGeneric: y])

    expectEqual(B_x_keypath(X.self), \B<X>.x)
    //TODO expectEqual(B_y_keypath(X.self), \B<X>.y)
    //TODO expectEqual(B_z_keypath(X.self), \B<X>.z)
    expectEqual(B_subscript_withInt_keypath(X.self, index: 0),
                \B<X>.[withInt: 0])
    expectEqual(B_subscript_withGeneric_keypath(X.self, index: y),
                \B<X>.[withGeneric: y])

    expectEqual(B_storedA_keypath(X.self), \B<X>.storedA)
    expectEqual(B_storedB_keypath(X.self), \B<X>.storedB)
  }

  testInGenericContext(x: 0.0, y: 42)
  testInGenericContext(x: "pomeranian", y: "big butt")
}

runAllTests()
