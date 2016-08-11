// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

func localFunc(_ x: Int) -> Int {
  func addToX(_ y: Int) -> Int {
    return x + y
  }
  return addToX(1)
}

func localFunc2(_ x: Int) -> (_ y: Int) -> Int {
  func addToX(_ y: Int) -> Int {
    return x + y
  }
  return addToX
}

// FIXME global vars
func test() {
  // CHECK: 3
  print(localFunc(2))
  // CHECK: 5
  print(localFunc2(2)(3))

  var lf = localFunc
  // CHECK: 8
  print(lf(7))

  var lf2 = localFunc2
  var lf2_ = lf2(5)
  // CHECK: 13
  print(lf2_(8))
}

test()

// <rdar://problem/19776288>
func map<T>(_ fn: (T) -> ()) {
    print("Void overload")
}

func map<T,U>(_ fn: (T) -> U) {
    print("Non-void overload")
}

map({()})
// CHECK: Void overload

map({(x: Int) -> Int in x})
// CHECK: Non-void overload
