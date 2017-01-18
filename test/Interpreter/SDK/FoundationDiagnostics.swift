// RUN: %target-swift-frontend %s -typecheck -verify

// REQUIRES: objc_interop

import Foundation

func useUnavailable() {
  var a = NSSimpleCString() // expected-error {{'NSSimpleCString' is unavailable}}
  var b = NSConstantString() // expected-error {{'NSConstantString' is unavailable}}
}

func encode(_ string: String) {
  _ = string.cString(using: NSUTF8StringEncoding) // expected-error {{'NSUTF8StringEncoding' has been renamed to 'String.Encoding.utf8'}} {{29-49=String.Encoding.utf8}}
  let _: NSStringEncoding? = nil // expected-error {{'NSStringEncoding' has been renamed to 'String.Encoding'}} {{10-26=String.Encoding}}
}
