// RUN: %target-typecheck-verify-swift -enable-experimental-feature MoveOnlyEnumDeinits

// REQUIRES: objc_interop
// REQUIRES: swift_feature_MoveOnlyEnumDeinits

// Validate that we can't mark an objc enum as move only.

@objc enum Foo : Int, ~Copyable { // expected-error {{noncopyable enums cannot be marked '@objc'}}
                       // expected-error@-1 {{'Foo' declares raw type 'Int', but cannot yet conform to RawRepresentable because it is noncopyable}}
  case X, Y, Z
  deinit {}
}

@objc enum Foo2 : Int, ~Copyable { // expected-error {{noncopyable enums cannot be marked '@objc'}}
                        // expected-error@-1 {{'Foo2' declares raw type 'Int', but cannot yet conform to RawRepresentable because it is noncopyable}}
  case X, Y, Z
}

