// RUN: %target-typecheck-verify-swift

@_frozen public enum Exhaustive {} // expected-warning {{@_frozen has no effect without -enable-resilience}} {{1-10=}}

@_frozen enum NotPublic {} // expected-warning {{@_frozen has no effect without -enable-resilience}} {{1-10=}}
