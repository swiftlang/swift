// RUN: %target-typecheck-verify-swift -warnings-as-errors

@frozen public enum Exhaustive {} // expected-no-warning

@frozen enum NotPublic {} // expected-no-warning
