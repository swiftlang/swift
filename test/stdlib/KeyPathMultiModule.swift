// -- Test with resilience enabled
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -force-single-frontend-invocation -Xfrontend -enable-key-path-resilience -Xfrontend -enable-resilience -module-name KeyPathMultiModule_b -c -o %t/KeyPathMultiModule_b.o -emit-module-path %t/KeyPathMultiModule_b.swiftmodule -parse-as-library %S/Inputs/KeyPathMultiModule_b.swift
// RUN: %target-build-swift -Xfrontend -enable-key-path-resilience %t/KeyPathMultiModule_b.o -I %t %s -o %t/a.out
// RUN: %target-run %t/a.out

// -- Test again with resilience disabled, which changes the circumstances under
// which we emit and use property descriptors
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -force-single-frontend-invocation -Xfrontend -enable-key-path-resilience -module-name KeyPathMultiModule_b -c -o %t/KeyPathMultiModule_b.o -emit-module-path %t/KeyPathMultiModule_b.swiftmodule -parse-as-library %S/Inputs/KeyPathMultiModule_b.swift
// RUN: %target-build-swift -Xfrontend -enable-key-path-resilience %t/KeyPathMultiModule_b.o -I %t %s -o %t/a.out
// RUN: %target-run %t/a.out

import KeyPathMultiModule_b
import StdlibUnittest

var keyPathMultiModule = TestSuite("key paths across multiple modules")

keyPathMultiModule.test("identity across multiple modules") {
  expectEqual(A_x_keypath(), \A.x)
  expectEqual(A_subscript_withGeneric_keypath(index: 0), \A.[withGeneric: 0])
  expectEqual(A_subscript_withGeneric_keypath(index: "butt"),
              \A.[withGeneric: "butt"])
  expectEqual(A_subscript_withGeneric_butt_keypath(),
              \A.[withGeneric: "pomeranian's big butt"])

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
}

runAllTests()
