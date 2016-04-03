// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Saturable: Comparable {
  func saturated(_ max: Self) -> Self
}

extension Int: Saturable {
  func saturated(_ max: Int) -> Int {
    return self > max ? max : self
  }
}

// CHECK-NOT: sil_witness_table Int: Equatable module witnesses_refinement { 
// CHECK-NOT: sil_witness_table Int: Comparable module witnesses_refinement { 
// CHECK: sil_witness_table hidden Int: Saturable module witnesses_refinement { 
