
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
