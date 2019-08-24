// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -c -verify -verify-ignore-unknown %s -o /dev/null

// REQUIRES: objc_interop

import ObjectiveC

class Foo: NSObject {
  override var hashValue: Int {
    // expected-error@-1 {{'NSObject.hashValue' is not overridable; did you mean to override 'NSObject.hash'?}}
    return 0
  }

  override func hash(into hasher: inout Hasher) {
    // expected-error@-1 {{overriding non-open instance method outside of its defining module}}
    // expected-error@-2 {{overriding declarations in extensions is not supported}}
  }
}
