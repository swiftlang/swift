// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: rdar42744631
// REQUIRES: tools-release,no_asserts

struct S {
  var A: [[UInt32]]

  func rdar32034560(x: UInt32) -> UInt32 {
    return ((self.A[0][Int(x >> 24) & 0xFF] &+ self.A[1][Int(x >> 16) & 0xFF]) ^ self.A[2][Int(x >> 8) & 0xFF]) &+ self.A[3][Int(x & 0xFF)]
         | ((self.A[0][Int(x >> 24) & 0xFF] &+ self.A[1][Int(x >> 16) & 0xFF]) ^ self.A[2][Int(x >> 8) & 0xFF]) &+ self.A[3][Int(x & 0xFF)]
         | ((self.A[0][Int(x >> 24) & 0xFF] &+ self.A[1][Int(x >> 16) & 0xFF]) ^ self.A[2][Int(x >> 8) & 0xFF]) &+ self.A[3][Int(x & 0xFF)]
         | ((self.A[0][Int(x >> 24) & 0xFF] &+ self.A[1][Int(x >> 16) & 0xFF]) ^ self.A[2][Int(x >> 8) & 0xFF]) &+ self.A[3][Int(x & 0xFF)]
   // expected-error@-1 {{reasonable time}}
  }
}
