// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s

// REQUIRES: objc_interop

import Foundation
import simd

@objc class Foo: NSObject {
  @objc func doStuffWithFloat4(_ x: float4) -> float4 { return x }
  @objc func doStuffWithDouble2(_ x: double2) -> double2 { return x }
  @objc func doStuffWithInt3(_ x: int3) -> int3 { return x }
  @objc func doStuffWithUInt4(_ x: uint4) -> uint4 { return x }
}

