// RUN: %swift %s -i | FileCheck %s
// XFAIL: *
// <rdar://problem/12940505> Default constructors not called
struct S {
  var val : Int
  constructor() {
    val = 42
  }
}

class C {
  var val : Int
  constructor() {
    val = 24
  }
}

func test() {
  var s1 : S
  var s2 = S()
  var c1 = new C
  var c2 = new C()
  print("\(s1.val) \(s2.val) \(c1.val) \(c2.val)\n")
  // CHECK: 42 42 24 24
}
test()
