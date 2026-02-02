// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: objc_interop

import Foundation

public func character(x: UniChar, y: UniChar) -> UTF32Char {
  return UTF32Char(((x - 0xd800) << 10) + (y - 0xdc00) + 0x0010000)
}
