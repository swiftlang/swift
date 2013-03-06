// RUN: %swift -sil-i %s | FileCheck %s

func localFunc(x:Int) -> Int {
  func addToX(y:Int) -> Int {
    return x + y
  }
  return addToX(1)
}

// CHECK: 3
println(localFunc(2))
