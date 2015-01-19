// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Saturable: Comparable {
  func saturated(max: Self) -> Self
}

extension Int: Saturable {
  func saturated(max: Int) -> Int {
    return self > max ? max : self
  }
}

// FIXME rdar://problem/18182969 -- we should not be producing
// sil_witness_tables for Int:Comparable and Int:Equatable, since the stdlib
// already has those.

// FIXME-NOT CHECK: sil_witness_table Int: Equatable module witnesses_refinement { 
// FIXME-NOT CHECK: sil_witness_table Int: Comparable module witnesses_refinement { 
// CHECK: sil_witness_table hidden Int: Saturable module witnesses_refinement { 
