// RUN: %target-typecheck-verify-swift

@unsafe func f() { }
// expected-error@-1{{attribute requires '-enable-experimental-feature AllowUnsafeAttribute'}}

