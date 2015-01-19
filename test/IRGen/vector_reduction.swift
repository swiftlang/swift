// RUN: %target-swift-frontend -Ounchecked %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64

// We were missing target transform info and not vectorizing the loop below.

// CHECK: xor <2 x i64>

func f() -> Int {
  var a : Int = 10
  var b : Int = 15
  var c : Int = 0

  for _ in 0..<100000 {
    for _ in 0..<1000000 {
      c = a ^ b ^ c
    }
  }

  return c
}

var c = f()
