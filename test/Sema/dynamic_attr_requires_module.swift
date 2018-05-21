// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s
// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s -parse-as-library

class Oof {
  dynamic func impliesObjC() { } // expected-error {{'dynamic' attribute used without importing module 'Foundation'}}
}
