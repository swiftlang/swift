// RUN: %target-typecheck-verify-swift

// References to associated type from 'where' clause should be
// ambiguous when there's a typealias with the same name in another
// unrelated protocol.

protocol P1 {
  typealias T = Int // expected-note 3{{found this candidate}}
}

protocol P2 {
  associatedtype T // expected-note 3{{found this candidate}}
}

extension P1 where Self : P2, T == Int { // expected-error {{'T' is ambiguous for type lookup in this context}}
  func takeT11(_: T) {} // expected-error {{'T' is ambiguous for type lookup in this context}}
  func takeT12(_: Self.T) {}
}

extension P1 where Self : P2 {
  // FIXME: This doesn't make sense -- either both should
  // succeed, or both should be ambiguous.
  func takeT21(_: T) {} // expected-error {{'T' is ambiguous for type lookup in this context}}
  func takeT22(_: Self.T) {}
}

// Same as above, but now we have two visible associated types with the same
// name.
protocol P3 {
  associatedtype T
}

extension P2 where Self : P3, T == Int {
  func takeT31(_: T) {}
  func takeT32(_: Self.T) {}
}

extension P2 where Self : P3 {
  func takeT41(_: T) {}
  func takeT42(_: Self.T) {}
}

protocol P4 : P2, P3 {
  func takeT51(_: T)
  func takeT52(_: Self.T)
}
