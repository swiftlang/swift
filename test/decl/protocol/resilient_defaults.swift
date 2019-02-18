// RUN: %target-typecheck-verify-swift -enable-resilience

public struct Wrapper<T: P>: P { }
extension Wrapper: Q where T: Q { }

public protocol PBase {
  associatedtype AssocType
}

public protocol P: PBase {
  override associatedtype AssocType: P = Wrapper<Self>
  // expected-note@-1{{associated type 'AssocType' has default type 'Wrapper<Self>' written here}}
}

public protocol Q: P where Self.AssocType: Q { }

public protocol R: Q where Self.AssocType: R { }
// expected-warning@-1{{default type 'Wrapper<Self>' for associated type 'AssocType' does not satisfy constraint 'Self.AssocType': 'R'}}
