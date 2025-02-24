// RUN: %empty-directory(%t/cache)
// RUN: not --crash %target-swift-frontend -c %s -module-cache-path %t/cache 2>&1 | %FileCheck %s
// CHECK: *** DESERIALIZATION FAILURE ***
// CHECK: SILFunction type mismatch for 'asinf': '$@convention(thin) (Float) -> Float' != '$@convention(c) (Float) -> Float'

// REQUIRES: VENDOR=apple

import Darwin

@_silgen_name("asinf") internal func quux(_ x: Float) -> Float

public func bar(_ x: Float) -> Float {
  return quux(x)
}
