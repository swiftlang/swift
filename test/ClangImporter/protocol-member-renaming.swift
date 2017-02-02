// RUN: %target-swift-frontend -typecheck -import-objc-header %S/Inputs/protocol-member-renaming.h -verify %s -verify-ignore-unknown

// REQUIRES: objc_interop

class Modern : NSObject, FooDelegate {
  func foo(_ foo: Foo, willConsume object: Any) {}
}

class PreMigration : NSObject, FooDelegate {
  func foo(_ foo: Foo, willConsumeObject object: Any) {}
  // expected-error@-1 {{'foo(_:willConsumeObject:)' has been renamed to 'foo(_:willConsume:)'}} {{24-41=willConsume}}
  // expected-error@-2 {{method 'foo(_:willConsumeObject:)' has different argument names from those required by protocol 'FooDelegate' ('foo(_:willConsume:)')}} {{24-41=willConsume}}
}

class OptionalButUnavailableImpl : OptionalButUnavailable {
  // Note the argument label that causes this not to match the requirement.
  func doTheThing(object: Any) {} // no-warning
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: 'foo(_:willConsumeObject:)' was obsoleted in Swift 3
