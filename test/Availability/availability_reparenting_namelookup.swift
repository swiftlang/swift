// RUN: %target-typecheck-verify-swift  -enable-experimental-feature Reparenting

// REQUIRES: swift_feature_Reparenting


@reparentable
@available(macOS 75, *)
public protocol BorrowSeq<Element> {
  associatedtype Element
}

public protocol Seq<Element>: BorrowSeq {
  associatedtype Element
}

@available(macOS 75, *)
extension Seq: @reparented BorrowSeq {}

public protocol Coll<Element>: Seq {
  override associatedtype Element
}

public protocol LCP: Coll, Seq {}

// This unqualified reference to Element should not resolve to BorrowSeq.Element
extension LCP where Element: Sendable {}
