// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Onone %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefix CHECK --check-prefix CHECK-ONONE %s
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-codesign %t/a.out.optimized
// RUN: %target-run %t/a.out.optimized | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

protocol P {}
@objc protocol PObjC {}
struct PS: P {}
enum PE: P {}
class PC: P, PObjC {}
class PCSub: PC {}

func nongenericAnyIsPObjC(type: Any.Type) -> Bool {
  return type is PObjC.Type
}
func genericAnyIs<T>(type: Any.Type, to: T.Type, expected: Bool) -> Bool {
  // If we're testing against a runtime that doesn't have the fix this tests,
  // just pretend we got it right.
  if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
    return type is T.Type
  } else {
    return expected
  }
}

// CHECK-LABEL: casting types to ObjC protocols with generics:
print("casting types to ObjC protocols with generics:")
print(nongenericAnyIsPObjC(type: PS.self)) // CHECK: false
print(genericAnyIs(type: PS.self, to: PObjC.self, expected: false)) // CHECK: false
print(nongenericAnyIsPObjC(type: PE.self)) // CHECK: false
print(genericAnyIs(type: PE.self, to: PObjC.self, expected: false)) // CHECK: false
print(nongenericAnyIsPObjC(type: PC.self)) // CHECK: true
print(genericAnyIs(type: PC.self, to: PObjC.self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyIsPObjC(type: PCSub.self)) // CHECK: true
print(genericAnyIs(type: PCSub.self, to: PObjC.self, expected: true)) // CHECK-ONONE: true
