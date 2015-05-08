// RUN: %target-run-simple-swift | FileCheck %s

func localFunc(x: Int) -> Int {
  func addToX(y: Int) -> Int {
    return x + y
  }
  return addToX(1)
}

func localFunc2(x: Int) -> (y: Int) -> Int {
  func addToX(y: Int) -> Int {
    return x + y
  }
  return addToX
}

// FIXME global vars
func test() {
  // CHECK: 3
  print(localFunc(2))
  // CHECK: 5
  print(localFunc2(2)(y: 3))

  var lf = localFunc
  // CHECK: 8
  print(lf(7))

  var lf2 = localFunc2
  var lf2_ = lf2(5)
  // CHECK: 13
  print(lf2_(y: 8))
}

test()

// <rdar://problem/19776288>
func map<T>(fn: T->()) {
    print("Void overload")
}

func map<T,U>(fn: T->U) {
    print("Non-void overload")
}

map({()})
// CHECK: Void overload

map({(x: Int) -> Int in x})
// CHECK: Non-void overload
