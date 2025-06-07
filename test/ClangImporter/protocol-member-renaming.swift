// RUN: %target-swift-frontend -typecheck -import-objc-header %S/Inputs/protocol-member-renaming.h -verify %s

// REQUIRES: objc_interop

class Modern : NSObject, FooDelegate {
  func foo(_ foo: Foo, willConsume object: Any) {}
}

class PreMigration : NSObject, FooDelegate {
  func foo(_ foo: Foo, willConsumeObject object: Any) {}
  // expected-error@-1 {{'foo(_:willConsumeObject:)' has been renamed to 'foo(_:willConsume:)'}} {{24-41=willConsume}}
}

class OptionalButUnavailableImpl : OptionalButUnavailable {
  // Note the argument label that causes this not to match the requirement.
  func doTheThing(object: Any) {} // no-warning
}
