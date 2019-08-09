// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s
// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s -parse-as-library

class Oof {
  @objc dynamic func impliesObjC() { }
  // expected-error@-1 {{@objc attribute used without importing module 'Foundation'}}
}
