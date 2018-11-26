// RUN: %target-typecheck-verify-swift -enable-resilience

@_frozen public enum Exhaustive {} // no-warning

@_frozen enum NotPublic {} // expected-warning {{@_frozen has no effect on non-public enums}} {{1-10=}}

internal enum Outer {
  @_frozen public enum ButThisIsOK {} // no-warning
}

@_frozen @usableFromInline enum NotPublicButVersioned {} // no-warning
