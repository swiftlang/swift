// RUN: %target-typecheck-verify-swift -swift-version 5

private typealias PrivateAlias = Int
// expected-note@-1 {{type alias 'PrivateAlias' is not '@usableFromInline' or public}}

internal typealias InternalAlias = Int
// expected-note@-1 {{type alias 'InternalAlias' is not '@usableFromInline' or public}}

@usableFromInline typealias UsableFromInlineAlias = Int

public typealias PublicAlias = Int

@inlinable public func f() {
  _ = PrivateAlias.self
  // expected-error@-1 {{type alias 'PrivateAlias' is private and cannot be referenced from an '@inlinable' function}}

  _ = InternalAlias.self
  // expected-error@-1 {{type alias 'InternalAlias' is internal and cannot be referenced from an '@inlinable' function}}

  _ = UsableFromInlineAlias.self

  _ = PublicAlias.self
}
