// RUN: %target-typecheck-verify-swift

@frozen public enum Exhaustive {} // expected-no-warning

@frozen enum NotPublic {} // expected-warning{{@frozen has no effect on non-public enums}}
