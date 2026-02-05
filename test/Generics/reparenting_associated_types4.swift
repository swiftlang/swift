// RUN: %target-typecheck-verify-swift -enable-experimental-feature Reparenting
// RUN: %target-swift-frontend %s -emit-irgen -enable-experimental-feature Reparenting > /dev/null

// REQUIRES: swift_feature_Reparenting

// Ensures that 'BorrowingSeq.Zelement' CAN be witnessed by 'Seq.Element',
// with the same-type requirement, because the string "Zelement" is after than "Element" lexicographically.
@reparentable
public protocol BorrowingSeq<Zelement> {
  associatedtype Zelement
}
public protocol Seq {
  associatedtype Element
}
extension Seq: @reparented BorrowingSeq
  where Zelement == Element {}



@reparentable
public protocol Door {
  associatedtype Key
}


public protocol FixedKey {}
extension FixedKey: @reparented Door where Key == String {}


public protocol BoundGenericKey {
  associatedtype MyKey
}
struct Wrapper<T> {}
extension BoundGenericKey: @reparented Door where Key == Wrapper<MyKey> {}
