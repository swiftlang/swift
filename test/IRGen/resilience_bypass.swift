// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/first.swiftmodule -module-name=first %S/Inputs/resilience_bypass/first.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/second.swiftmodule -module-name=second %S/Inputs/resilience_bypass/second.swift -I %t
// RUN: %target-swift-frontend -emit-ir -enable-resilience-bypass %s -I %t | %FileCheck %s -DINT=i%target-ptrsize

import second

// CHECK:       define{{( protected)?}} swiftcc [[INT]] @"$S17resilience_bypass7getSizeSiyF"() {{.*}} {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    ret [[INT]] {{5|9}}
// CHECK-NEXT:  }

public func getSize() -> Int {
  return MemoryLayout<E>.size
}
