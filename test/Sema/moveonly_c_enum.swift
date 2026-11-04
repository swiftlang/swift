// RUN: %target-typecheck-verify-swift -enable-experimental-feature MoveOnlyEnumDeinits -disable-objc-interop

// REQUIRES: swift_feature_MoveOnlyEnumDeinits

// Validate that we can't mark a @c enum as move only.

@c enum Foo : Int, ~Copyable { // expected-error {{non-'Copyable' enums cannot be marked '@c'}}
                       // expected-error@-1 {{'Foo' declares raw type 'Int', but cannot yet conform to RawRepresentable because it is non-'Copyable'}}
  case X, Y, Z
  deinit {}
}

@c enum Foo2 : Int, ~Copyable { // expected-error {{non-'Copyable' enums cannot be marked '@c'}}
                        // expected-error@-1 {{'Foo2' declares raw type 'Int', but cannot yet conform to RawRepresentable because it is non-'Copyable'}}
  case X, Y, Z
}
