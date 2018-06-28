// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s -swift-version 3
// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s -parse-as-library -swift-version 3

class Oof {
  dynamic func impliesObjC() { } // expected-error {{'dynamic' attribute used without importing module 'Foundation'}}
}
