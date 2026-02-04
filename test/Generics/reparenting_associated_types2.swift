// RUN: %target-typecheck-verify-swift -enable-experimental-feature Reparenting -dump-requirement-machine 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_Reparenting

// This test ensures that BorrowingSeq.Element reduces to Sequence.Element in the extension's context.

@reparentable
public protocol BorrowingSeq<Element> {
  associatedtype Element
}
public protocol Seq {
  associatedtype Element
}
extension Seq: @reparented BorrowingSeq {}

// CHECK: Adding generic signature <τ_0_0 where τ_0_0 : Seq>
// CHECK: Rewrite system: {
// CHECK: - [Seq].[BorrowingSeq:Element] => [Seq:Element]
// CHECK: - τ_0_0.Element => τ_0_0.[Seq:Element]
// CHECK: - τ_0_0.[BorrowingSeq:Element] => τ_0_0.[Seq:Element]
// CHECK: }



