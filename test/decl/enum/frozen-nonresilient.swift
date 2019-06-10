// RUN: %target-typecheck-verify-swift

@frozen public enum Exhaustive {} // expected-warning {{@frozen has no effect without -enable-library-evolution}} {{1-9=}}

@frozen enum NotPublic {} // expected-warning {{@frozen has no effect without -enable-library-evolution}} {{1-9=}}
