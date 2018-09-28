// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

extension Collection {
  func foo() {
    // CHECK: witness_method $Self.Indices, #Sequence.dropFirst!1
    _ = zip(indices, indices.dropFirst(3))
  }
}
