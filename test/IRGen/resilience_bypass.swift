// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/first.swiftmodule -module-name=first %S/Inputs/resilience_bypass/first.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/second.swiftmodule -module-name=second %S/Inputs/resilience_bypass/second.swift -I %t
// RUN: %target-swift-frontend -emit-ir -enable-resilience-bypass %s -I %t | %FileCheck %s -DINT=i%target-ptrsize

import first
import second

// CHECK:       define{{( dllexport| protected)?}} swiftcc [[INT]] @"$S17resilience_bypass7getSizeSiyF"() {{.*}} {
// CHECK-NEXT:  entry:

// FIXME: The metadata accessor call is not necessary
// CHECK-NEXT:    [[RESPONSE:%.*]] = call swiftcc %swift.metadata_response @"$S6second1EOMa"
// CHECK-NEXT:    [[METADATA:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 0

// CHECK-NEXT:    ret [[INT]] {{5|9}}
// CHECK-NEXT:  }

public func getSize() -> Int {
  return MemoryLayout<E>.size
}

public func makeE(_ s: S) -> E {
  return .b(s)
}

// CHECK:       define{{( dllexport| protected)?}} swiftcc void @"$S17resilience_bypass7makeAnyyyp5first1SVF"(
// CHECK-NEXT:  entry:

// Make sure we form the existential using the result of the metadata accessor, instead of
// directly using the address of the metadata global.
//
// FIXME: The metadata global should have hidden linkage to rule out this kind of bug.

// CHECK-NEXT:    [[RESPONSE:%.*]] = call swiftcc %swift.metadata_response @"$S6second1EOMa"

public func makeAny(_ s: S) -> Any {
  return makeE(s)
}
