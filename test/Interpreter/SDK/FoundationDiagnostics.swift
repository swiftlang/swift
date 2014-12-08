// RUN: %target-swift-frontend %s -parse -verify

import Foundation

func useUnavailable() {
  var a = NSSimpleCString() // expected-error {{'NSSimpleCString' is unavailable}}
  var b = NSConstantString() // expected-error {{'NSConstantString' is unavailable}}
}
