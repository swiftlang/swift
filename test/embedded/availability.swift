// RUN: %target-typecheck-verify-swift -parse-stdlib -enable-experimental-feature Embedded

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

@_unavailableInEmbedded
public struct UnavailableInEmbedded {}
// expected-note@-1 {{'UnavailableInEmbedded' has been explicitly marked unavailable here}}

@available(*, unavailable, message: "always unavailable")
public struct UniverallyUnavailable {}
// expected-note@-1 3 {{'UniverallyUnavailable' has been explicitly marked unavailable here}}

@_unavailableInEmbedded
public func unavailable_in_embedded() { }
// expected-note@-1 {{'unavailable_in_embedded()' has been explicitly marked unavailable here}}

@available(*, unavailable, message: "always unavailable")
public func universally_unavailable() { }
// expected-note@-1 4 {{'universally_unavailable()' has been explicitly marked unavailable here}}

@_unavailableInEmbedded
public func unused() { } // no error

public struct S1 {} // expected-note 2 {{found this candidate}}
public struct S2 {} // expected-note 2 {{found this candidate}}

@_unavailableInEmbedded
public func has_unavailable_in_embedded_overload(_ s1: S1) { }

public func has_unavailable_in_embedded_overload(_ s2: S2) { }

@available(*, unavailable)
public func has_universally_unavailable_overload(_ s1: S1) { }

public func has_universally_unavailable_overload(_ s2: S2) { }

public struct Available {}

@_unavailableInEmbedded
extension Available {
  public func unavailable_in_embedded_method( // expected-note {{'unavailable_in_embedded_method' has been explicitly marked unavailable here}}
    _ uie: UnavailableInEmbedded,
    _ uu: UniverallyUnavailable, // expected-error {{'UniverallyUnavailable' is unavailable: always unavailable}}
    _ a: Available,
  ) {
    unavailable_in_embedded()
    universally_unavailable() // expected-error {{'universally_unavailable()' is unavailable: always unavailable}}
    a.unavailable_in_embedded_method(uie, uu, a)
  }
}

public func available(
  _ uie: UnavailableInEmbedded, // expected-error {{'UnavailableInEmbedded' is unavailable: unavailable in embedded Swift}}
  _ uu: UniverallyUnavailable, // expected-error {{'UniverallyUnavailable' is unavailable: always unavailable}}
  _ a: Available,
) {
  unavailable_in_embedded() // expected-error {{'unavailable_in_embedded()' is unavailable: unavailable in embedded Swift}}
  universally_unavailable() // expected-error {{'universally_unavailable()' is unavailable: always unavailable}}
  a.unavailable_in_embedded_method(uie, uu, a) // expected-error {{'unavailable_in_embedded_method' is unavailable: unavailable in embedded Swift}}
  has_unavailable_in_embedded_overload(.init())
  has_universally_unavailable_overload(.init()) // not ambiguous, selects available overload
}

@_unavailableInEmbedded
public func also_unavailable_in_embedded(
  _ uie: UnavailableInEmbedded, // OK
  _ uu: UniverallyUnavailable, // expected-error {{'UniverallyUnavailable' is unavailable: always unavailable}}
  _ a: Available,
) {
  unavailable_in_embedded() // OK
  universally_unavailable() // expected-error {{'universally_unavailable()' is unavailable: always unavailable}}
  a.unavailable_in_embedded_method(uie, uu, a)
  has_unavailable_in_embedded_overload(.init()) // expected-error {{ambiguous use of 'init()'}}
  has_universally_unavailable_overload(.init()) // not ambiguous, selects available overload
}

@available(*, unavailable)
public func also_universally_unavailable(
  _ uie: UnavailableInEmbedded, // OK
  _ uu: UniverallyUnavailable, // OK
  _ a: Available,
) {
  unavailable_in_embedded()
  universally_unavailable() // expected-error {{'universally_unavailable()' is unavailable: always unavailable}}
  a.unavailable_in_embedded_method(uie, uu, a)
  has_unavailable_in_embedded_overload(.init()) // expected-error {{ambiguous use of 'init()'}}
  has_universally_unavailable_overload(.init()) // not ambiguous, selects available overload
}

class Base {
  func alwaysAvailable() { }

  @_unavailableInEmbedded
  func overrideAsUnavailable() { }

  @_unavailableInEmbedded
  func overrideLessUnavailable() { } // expected-note {{'overrideLessUnavailable()' has been explicitly marked unavailable here}}

  func overrideMoreUnavailable() { } // expected-note {{overridden declaration is here}}
}

class DerivedAvailable: Base {
  override func alwaysAvailable() { }

  @_unavailableInEmbedded
  override func overrideAsUnavailable() { }

  override func overrideLessUnavailable() { } // expected-error {{cannot override 'overrideLessUnavailable' which has been marked unavailable}}
  // expected-note@-1 {{remove 'override' modifier to declare a new 'overrideLessUnavailable'}}

  @_unavailableInEmbedded
  override func overrideMoreUnavailable() { } // expected-error {{cannot override 'overrideMoreUnavailable' with a declaration that is marked unavailable}}{{112:3-26=}}
}

@_unavailableInEmbedded
class DerivedUnavailable: Base {
  override func alwaysAvailable() { }

  @_unavailableInEmbedded
  override func overrideAsUnavailable() { }

  override func overrideLessUnavailable() { }

  @_unavailableInEmbedded
  override func overrideMoreUnavailable() { }
}
