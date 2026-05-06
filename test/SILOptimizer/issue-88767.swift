// RUN: %target-swift-frontend -emit-sil -verify %s
// RUN: %target-swift-frontend -O -emit-sil -verify %s

// REQUIRES: objc_interop

// Regression test for https://github.com/swiftlang/swift/issues/88767

import Foundation

extension NSDecimalNumber {
  static func roundtrip(_ d: Decimal) -> Self? {
    return d as? Self
  }
}

extension NSString {
  static func roundtrip(_ s: String) -> Self? {
    return s as? Self
  }
}
