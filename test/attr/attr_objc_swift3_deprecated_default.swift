// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s -swift-version 3
// REQUIRES: objc_interop

import Foundation

class C : NSObject { }

extension C {
  func foo() { }
}

class D : C { }

extension D {
  override func foo() { }  // do not warn
}
