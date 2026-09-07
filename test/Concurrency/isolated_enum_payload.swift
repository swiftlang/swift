// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 %s

// REQUIRES: concurrency

// 'isolated' is not allowed on an enum case payload. Building the element's
// constructor type used to trip FunctionType's isolation invariant and crash
// before any of these diagnostics could be emitted.
// https://github.com/swiftlang/swift/issues/86007

enum E {
  case beforeName(isolated Int)
  // expected-error@-1 {{'isolated' before a parameter name is not allowed, place it before the parameter type instead}}
  // expected-error@-2 {{'isolated' may only be used on parameters}}

  case inTypePosition(x: isolated Int)
  // expected-error@-1 {{'isolated' may only be used on parameters}}

  case actorPayload(isolated any Actor)
  // expected-error@-1 {{'isolated' before a parameter name is not allowed, place it before the parameter type instead}}
  // expected-error@-2 {{'isolated' may only be used on parameters}}

  case ok(Int)
}
