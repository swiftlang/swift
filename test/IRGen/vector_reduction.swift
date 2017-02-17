// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -Ounchecked %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

// rdar://30579970
// REQUIRES: optimized_stdlib

// FIXME: https://bugs.swift.org/browse/SR-2808
// XFAIL: resilient_stdlib

// We were missing target transform info and not vectorizing the loop below.

// CHECK: xor <2 x i64>

public func f(a: UnsafePointer<Int>, b: UnsafePointer<Int>, count: Int) -> Int {
  var c = 0

  for i in 0..<count {
    c = a[i] ^ b[i] ^ c
  }

  return c
}

