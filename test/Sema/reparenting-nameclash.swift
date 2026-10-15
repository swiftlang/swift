// RUN: %target-typecheck-verify-swift  -enable-experimental-feature Reparenting

// REQUIRES: swift_feature_Reparenting

public struct DefaultThing {}

@reparentable
@available(macOS 75, *)
public protocol BorrowSeq {
  associatedtype Thing = DefaultThing
}

public protocol Seq: BorrowSeq {}

@available(macOS 75, *)
extension Seq: @reparented BorrowSeq {}


// Pre-existing code in a client, rebuilt agianst a new library
// that did a reparenting:

public struct Thing {}

public struct Hello: Seq {
  public func whatever() -> Thing { return Thing() }

  // The above use to work, where Thing referred to the struct Thing above.
  // But associated type witness inference injected `typealias Thing = DefaultThing`
  // here, and that's bad!
}
