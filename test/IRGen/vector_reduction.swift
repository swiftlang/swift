// RUN: %target-swift-frontend -Ounchecked %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64

// We were missing target transform info and not vectorizing the loop below.

// CHECK: xor <2 x i64>

public func f(a: UnsafePointer<Int>, b: UnsafePointer<Int>, count: Int) -> Int {
  var c = 0

  for i in 0..<count {
    c = a[i] ^ b[i] ^ c
  }

  return c
}

