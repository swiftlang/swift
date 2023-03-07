// RUN: %target-typecheck-verify-swift -enable-experimental-move-only

// REQUIRES: objc_interop

// Validate that we can't mark an objc enum as move only.

@_moveOnly
@objc enum Foo : Int { // expected-error {{@objc enums cannot be marked as move-only}}
  case X, Y, Z
  deinit {} // objc enums cannot have deinits
}

@_moveOnly
@objc enum Foo2 : Int { // expected-error {{@objc enums cannot be marked as move-only}}
  case X, Y, Z
}

