/// rdar://69876253
// REQUIRES: VENDOR=apple
// RUN: %target-build-swift %s -o %t.out

import SwiftShims
import Foundation

func foo () {
  let a = UnsafeMutableRawPointer.allocate(byteCount: 4, alignment: 4)
  let b = UnsafeMutableRawPointer.allocate(byteCount: 4, alignment: 4)
  memcmp(a, b, 4)
}
