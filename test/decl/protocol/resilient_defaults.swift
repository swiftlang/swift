// RUN: %target-typecheck-verify-swift -enable-library-evolution -verify-additional-file Swift.Collection.SubSequence

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

public struct E {}
public protocol P1: RandomAccessCollection where SubSequence: P1 {}
// expected-warning@-1{{default type 'Slice<Self>' for associated type 'SubSequence' does not satisfy constraint 'Self.SubSequence': 'P1'}}

// expected-note@Swift.Collection.SubSequence:2 {{associated type 'SubSequence' has default type 'Slice<Self>' written here}}