// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s

// REQUIRES: objc_interop

import Foundation
import simd

@objc class Foo: NSObject {
  @objc func doStuffWithFloat4(x: float4) -> float4 { return x }
  @objc func doStuffWithDouble2(x: double2) -> double2 { return x }
  @objc func doStuffWithInt3(x: int3) -> int3 { return x }
}

