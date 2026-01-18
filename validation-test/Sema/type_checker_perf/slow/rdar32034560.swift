// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000
// REQUIRES: tools-release,no_asan

// Valid expression, type checks with default limits but slow

struct S {
  var A: [[UInt32]]

  func rdar32034560(x: UInt32) -> UInt32 {
    // expected-error@+1 {{reasonable time}}
    return ((self.A[0][Int(x >> 24) & 0xFF] &+ self.A[1][Int(x >> 16) & 0xFF]) ^ self.A[2][Int(x >> 8) & 0xFF]) &+ self.A[3][Int(x & 0xFF)]
         | ((self.A[0][Int(x >> 24) & 0xFF] &+ self.A[1][Int(x >> 16) & 0xFF]) ^ self.A[2][Int(x >> 8) & 0xFF]) &+ self.A[3][Int(x & 0xFF)]
         | ((self.A[0][Int(x >> 24) & 0xFF] &+ self.A[1][Int(x >> 16) & 0xFF]) ^ self.A[2][Int(x >> 8) & 0xFF]) &+ self.A[3][Int(x & 0xFF)]
         | ((self.A[0][Int(x >> 24) & 0xFF] &+ self.A[1][Int(x >> 16) & 0xFF]) ^ self.A[2][Int(x >> 8) & 0xFF]) &+ self.A[3][Int(x & 0xFF)]
  }
}
