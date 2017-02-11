// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s
// REQUIRES: objc_interop

// This test can't use '-verify' mode, because the potential error wouldn't
// belong to any file.
// e.g.:
//   <unknown>:0: error: type 'NSNotification.Name' does not conform to protocol 'Comparable'

import Foundation

func acceptEquatable<T: Equatable>(_: T) {}
func acceptHashable<T: Hashable>(_: T) {}
func acceptComparable<T: Comparable>(_: T) {}

func testNewTypeWrapperComparable(x: NSNotification.Name, y: NSNotification.Name) {
  acceptEquatable(x)
  acceptHashable(x)
  acceptComparable(x)

  _ = x == y
  _ = x != y
  _ = x.hashValue
  _ = x < y
  _ = x > y
  _ = x <= y
  _ = x >= y
  _ = x as NSString
}
