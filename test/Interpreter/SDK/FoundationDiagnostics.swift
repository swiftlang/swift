// RUN: %target-swift-frontend %s -parse -verify
// XFAIL: linux

import Foundation

func useUnavailable() {
  var a = NSSimpleCString() // expected-error {{'NSSimpleCString' is unavailable}} expected-error {{cannot convert the expression's type}}
  var b = NSConstantString() // expected-error {{'NSConstantString' is unavailable}} expected-error {{cannot convert the expression's type}}
}
