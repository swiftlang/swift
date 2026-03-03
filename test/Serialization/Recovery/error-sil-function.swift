// RUN: %empty-directory(%t/cache)
// RUN: %target-swift-frontend -c %s -module-cache-path %t/cache

// REQUIRES: VENDOR=apple

import Darwin

@_silgen_name("asinf") internal func quux(_ x: Float) -> Float

public func bar(_ x: Float) -> Float {
  return quux(x)
}
