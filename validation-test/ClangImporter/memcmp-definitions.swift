/// Darwin's memcmp accepts nullable pointers, make sure the SwiftShims one
/// preserves the same type.
// REQUIRES: VENDOR=apple
// RUN: %target-build-swift %s -o %t.out

import SwiftShims
import Foundation

func foo () {
  let a = UnsafeMutableRawPointer.allocate(byteCount: 4, alignment: 4)
  let b = UnsafeMutableRawPointer.allocate(byteCount: 4, alignment: 4)
  memcmp(a, b, 4)

  memcmp(nil, nil, 0)
}
