// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -sil-full-demangle %s -enable-sil-ownership

// REQUIRES: objc_interop

import Foundation

extension NSString {
  var x: Float { return 0.0 }
}

// note: this has to be a var
var str: String = "hello world"

// Crash when NSString member is accessed on a String with an lvalue base
_ = str.x
