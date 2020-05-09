// RUN: %target-typecheck-verify-swift

// Reference to associated type from 'where' clause should not be
// ambiguous when there's a typealias with the same name in another
// protocol.
//
// FIXME: The semantics here are still really iffy. There's also a
// case to be made that type aliases in protocol extensions should
// only act as defaults and not as same-type constraints. However,
// if we decide to go down that route, it's important that *both*
// the unqualified (T) and qualified (Self.T) lookups behave the
// same.
protocol P1 {
  typealias T = Int // expected-note {{found this candidate}}
}

protocol P2 {
  associatedtype T // expected-note {{found this candidate}}
}

// FIXME: This extension's generic signature is still minimized differently from
// the next one. We need to decide if 'T == Int' is a redundant requirement or
// not.
extension P1 where Self : P2, T == Int {
  func takeT1(_: T) {}
  func takeT2(_: Self.T) {}
}

extension P1 where Self : P2 {
  // FIXME: This doesn't make sense -- either both should
  // succeed, or both should be ambiguous.
  func takeT1(_: T) {} // expected-error {{'T' is ambiguous for type lookup in this context}}
  func takeT2(_: Self.T) {}
}

// Same as above, but now we have two visible associated types with the same
// name.
protocol P3 {
  associatedtype T
}

// FIXME: This extension's generic signature is still minimized differently from
// the next one. We need to decide if 'T == Int' is a redundant requirement or
// not.
extension P2 where Self : P3, T == Int {
  func takeT1(_: T) {}
  func takeT2(_: Self.T) {}
}

extension P2 where Self : P3 {
  func takeT1(_: T) {}
  func takeT2(_: Self.T) {}
}

protocol P4 : P2, P3 {
  func takeT1(_: T)
  func takeT2(_: Self.T)
}
