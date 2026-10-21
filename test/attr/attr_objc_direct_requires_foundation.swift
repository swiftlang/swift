// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// %target-typecheck-verify-swift passes -disable-objc-attr-requires-foundation-module,
// so spell the invocation out.
// RUN: %target-swift-frontend -typecheck -verify %s \
// RUN:   -enable-experimental-feature ObjCDirect

import ObjectiveC

class C: NSObject {
  @objcDirect func f() {}
  // expected-error@-1 {{@objcDirect attribute used without importing module 'Foundation'}}
}
