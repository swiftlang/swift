// RUN: %target-typecheck-verify-swift -enable-experimental-feature MoveOnlyEnumDeinits

// REQUIRES: objc_interop

// Validate that we can't mark an objc enum as move only.

@objc enum Foo : Int, ~Copyable { // expected-error {{@objc enums cannot be noncopyable}}
  case X, Y, Z
  deinit {} // expected-error {{deinitializers cannot be declared on an @objc enum type}}
}

@objc enum Foo2 : Int, ~Copyable { // expected-error {{@objc enums cannot be noncopyable}}
  case X, Y, Z
}

