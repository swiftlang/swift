// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

extension Collection {
  func foo() {
    // CHECK: witness_method $Self.Indices, #Sequence.dropFirst!1 : <Self where Self : Sequence> (Self) -> (Int) -> Self.SubSequence : $@convention(witness_method: Sequence) <τ_0_0 where τ_0_0 : Sequence> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0.SubSequence
    _ = zip(indices, indices.dropFirst(3))
  }
}
