// RUN: %target-typecheck-verify-swift

// None of this is enforced for now, but make sure we don't crash or
// do anything stupid when a typealias is annotated with @usableFromInline.

private typealias PrivateAlias = Int

internal typealias InternalAlias = Int

@usableFromInline typealias UsableFromInlineAlias = Int

public typealias PublicAlias = Int

@inlinable public func f() {
  _ = PrivateAlias.self

  _ = InternalAlias.self

  _ = UsableFromInlineAlias.self

  _ = PublicAlias.self
}
