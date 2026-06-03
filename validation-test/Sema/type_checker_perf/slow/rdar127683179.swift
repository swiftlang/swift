// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000

// REQUIRES: objc_interop

import Foundation  // this pulls in more overloads and makes it slower

// Invalid expression

func test(x: Double, y: Double, z: Int, b: Bool) {
  let _ = b ? (x + z, y + z) : (y + z, x + z)
  // expected-error@-1 {{reasonable time}}
}
