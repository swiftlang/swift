// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -c -verify -verify-ignore-unknown %s -o /dev/null

// REQUIRES: objc_interop

import ObjectiveC

class Foo: NSObject {
  override var hashValue: Int { // expected-error {{overriding non-open property outside of its defining module}} expected-error {{overriding non-@objc declarations from extensions is not supported}}
    return 0
  }

  override func hash(into hasher: inout Hasher) { // expected-error {{overriding non-open instance method outside of its defining module}} expected-error {{overriding declarations in extensions is not supported}}
  }
}
