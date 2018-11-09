// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-swift-frontend -typecheck -disable-access-control %s

public protocol PublicProtoWithReqs {
  associatedtype Assoc
  func foo()
}

@usableFromInline struct UFIAdopter<T> : PublicProtoWithReqs {}
// expected-error@-1 {{type alias 'Assoc' must be declared '@usableFromInline' because because it matches a requirement in protocol 'PublicProtoWithReqs'}} {{none}}
extension UFIAdopter {
  typealias Assoc = Int
  // expected-note@-1 {{'Assoc' declared here}}
  func foo() {}
}

@usableFromInline struct UFIAdopterAllInOne<T> : PublicProtoWithReqs {
  typealias Assoc = Int
  // expected-error@-1 {{type alias 'Assoc' must be declared '@usableFromInline' because because it matches a requirement in protocol 'PublicProtoWithReqs'}} {{none}}
  func foo() {}
}

internal struct InternalAdopter<T> : PublicProtoWithReqs {}
extension InternalAdopter {
  typealias Assoc = Int // okay
  func foo() {} // okay
}


@usableFromInline protocol UFIProtoWithReqs {
  associatedtype Assoc
  func foo()
}

public struct PublicAdopter<T> : UFIProtoWithReqs {}
// expected-error@-1 {{type alias 'Assoc' must be declared '@usableFromInline' because because it matches a requirement in protocol 'UFIProtoWithReqs'}} {{none}}
extension PublicAdopter {
  typealias Assoc = Int
  // expected-note@-1 {{'Assoc' declared here}}
  func foo() {}
}
extension InternalAdopter: UFIProtoWithReqs {} // okay
