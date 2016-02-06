// RUN: %target-swift-frontend -emit-silgen -enable-resilience %s | FileCheck %s --check-prefix=CHECK --check-prefix=GLOBAL

public protocol P {}

// Protocol is public -- needs resilient witness table
public protocol ResilientProtocol {
  associatedtype T : P
  func f()
  func g()
}

// Protocol is not public -- doesn't need default witness table
protocol InternalProtocol {
  func f()
}

// CHECK-LABEL: sil_default_witness_table P 0 {
// CHECK-NEXT: }

// CHECK-LABEL: sil_default_witness_table ResilientProtocol 4 {
// CHECK-NEXT: }

// GLOBAL-NOT: sil_default_witness_table InternalProtocol
