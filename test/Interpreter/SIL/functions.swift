// RUN: %swift -sil-i %s | FileCheck %s
func double(x:Int) -> Int {
  return x+x
}

func curriedSubtract(x:Int)(y:Int) -> Int {
  return x-y
}

// CHECK: 4
println(double(2))
// CHECK: 8
println(double(4))

// CHECK: 12
println(curriedSubtract(16)(4))
