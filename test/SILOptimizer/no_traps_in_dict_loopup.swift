// RUN: %target-swift-frontend -O -emit-ir -parse-as-library -primary-file %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib


// A dictionary lookup should not contain any trap. Nothing can go wrong
// from the user's perspective.

// CHECK-NOT: llvm.trap

public func testit(_ s: [Int : Int]) -> Int? {
  return s[27]
}

