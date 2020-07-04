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

func nongenericAnyIsPObjCType(type: Any.Type) -> Bool {
  return type is PObjC.Type
}
func nongenericAnyIsPObjCProtocol(type: Any.Type) -> Bool {
  return type is PObjC.Protocol
}
func genericAnyIs<T>(type: Any.Type, to: T.Type, expected: Bool) -> Bool {
  // If we're testing against a runtime that doesn't have the fix this tests,
  // just pretend we got it right.
  if #available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *) {
    return type is T.Type
  } else {
    return expected
  }
}

// CHECK-LABEL: casting types to ObjC protocol existential metatype:
print("casting types to ObjC protocol existential metatype:")
print(#line, nongenericAnyIsPObjCType(type: PS.self)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPObjCType(type: PE.self)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPObjCType(type: PC.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyIsPObjCType(type: PCSub.self)) // CHECK: [[@LINE]] true

// CHECK-LABEL: casting types to ObjC protocol metatype:
print("casting types to ObjC protocol metatype:")
print(#line, nongenericAnyIsPObjCProtocol(type: PS.self)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPObjCProtocol(type: PE.self)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPObjCProtocol(type: PC.self)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPObjCProtocol(type: PCSub.self)) // CHECK: [[@LINE]] false

// CHECK-LABEL: casting types to ObjC protocol metatype via generic:
print("casting types to ObjC protocol metatype via generic:")
print(#line, genericAnyIs(type: PS.self, to: PObjC.self, expected: false)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PE.self, to: PObjC.self, expected: false)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PC.self, to: PObjC.self, expected: false)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PCSub.self, to: PObjC.self, expected: false)) // CHECK: [[@LINE]] false
