// RUN: %swift -sil-i %s | FileCheck %s
// XFAIL: *

func localFunc(x:Int) -> Int {
  func addToX(y:Int) -> Int {
    return x + y
  }
  return addToX(1)
}
