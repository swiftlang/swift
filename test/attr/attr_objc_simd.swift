// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s

// REQUIRES: objc_interop

import Foundation
import simd

@objc class Foo: NSObject {
  @objc func doStuffWithFloat4(x: Float4) -> Float4 { return x }
  @objc func doStuffWithDouble2(x: Double2) -> Double2 { return x }
  @objc func doStuffWithInt3(x: Int3) -> Int3 { return x }
}

