// RUN: %target-typecheck-verify-swift -swift-version 6

internal typealias InternalAlias = Int // expected-note 2 {{type alias 'InternalAlias' is not '@usableFromInline' or public}}

@inlinable public func localTypealiases() {
  typealias LocalAlias = InternalAlias // expected-error {{type alias 'InternalAlias' is internal and cannot be referenced from an '@inlinable' function}}
  typealias GenericAlias<T> = (T, InternalAlias) // expected-error {{type alias 'InternalAlias' is internal and cannot be referenced from an '@inlinable' function}}
}
