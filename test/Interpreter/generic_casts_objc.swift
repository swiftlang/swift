// RUN: %target-run-simple-swift | %FileCheck --check-prefix CHECK --check-prefix CHECK-ONONE --dump-input fail %s
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
func genericAnyIs<T>(type: Any.Type, to: T.Type) -> Bool {
  return type is T.Type
}

// CHECK-LABEL: casting types to ObjC protocols with generics:
print("casting types to ObjC protocols with generics:")
print(nongenericAnyIsPObjC(type: PS.self)) // CHECK: false
print(genericAnyIs(type: PS.self, to: PObjC.self)) // CHECK: false
print(nongenericAnyIsPObjC(type: PE.self)) // CHECK: false
print(genericAnyIs(type: PE.self, to: PObjC.self)) // CHECK: false
print(nongenericAnyIsPObjC(type: PC.self)) // CHECK: true
print(genericAnyIs(type: PC.self, to: PObjC.self)) // CHECK-ONONE: true
print(nongenericAnyIsPObjC(type: PCSub.self)) // CHECK: true
print(genericAnyIs(type: PCSub.self, to: PObjC.self)) // CHECK-ONONE: true
