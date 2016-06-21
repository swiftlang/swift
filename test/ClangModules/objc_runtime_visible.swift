// RUN: %target-swift-frontend -parse -disable-objc-attr-requires-foundation-module -I %S/../Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

import ObjCRuntimeVisible

extension A {
  @objc func foo() { } // expected-error{{@objc is not supported within extensions of classes only visible via the Objective-C runtime}}
}

class B : A { } // expected-error{{inheritance from an Objective-C class 'A' only visible via the runtime}}
