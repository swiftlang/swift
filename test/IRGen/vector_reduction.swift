// We were missing target transform info and not vectorizing the loop below.
// RUN: %swift -target x86_64-apple-darwin10  -Ofast %s  -emit-ir | FileCheck %s
// REQUIRES: X86

// CHECK: xor <2 x i64>

func f() -> Int {
  var a : Int = 10
  var b : Int = 15
  var c : Int = 0

  for _ in 0..100000 {
    for _ in 0..1000000 {
      c = a ^ b ^ c
    }
  }

  return c
}

var c = f()
