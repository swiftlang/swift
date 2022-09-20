// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s
// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s -parse-as-library

@objcMembers class Oof {
  // expected-error@-1 {{@objcMembers attribute used without importing module 'Foundation'}}
}
