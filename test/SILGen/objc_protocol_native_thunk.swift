// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s

// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %s
// REQUIRES: objc_interop

import Foundation

@objc protocol P {
  func p(_: String)
}
@objc class C: NSObject {
  func c(_: String) {}
}

func foo(x: Bool, y: C & P) -> (String) -> () {
  return x ? y.c : y.p
}
