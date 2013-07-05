// RUN: %swift -i %s | FileCheck %s

func localFunc(x:Int) -> Int {
  func addToX(y:Int) -> Int {
    return x + y
  }
  return addToX(1)
}

func localFunc2(x:Int) -> (y:Int) -> Int {
  func addToX(y:Int) -> Int {
    return x + y
  }
  return addToX
}

// FIXME global vars
func test() {
  // CHECK: 3
  println(localFunc(2))
  // CHECK: 5
  println(localFunc2(2)(3))

  var lf = localFunc
  // CHECK: 8
  println(lf(7))

  var lf2 = localFunc2
  var lf2_ = lf2(5)
  // CHECK: 13
  println(lf2_(8))
}

test()
