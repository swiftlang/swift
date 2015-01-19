// RUN: %target-swift-frontend %s -emit-ir | FileCheck %s

// Multiple local decls.
func test1() {
  class a {
    var x, y : Int
    init() {
      x = 0
      y = 0
    }
  }
  class b {
    var a, b : Int
    init() {
      a = 0
      b = 0
    }
  }
  var _ : a, _ : b
}

// CHECK: private unnamed_addr constant [2 x i8] c"a\00"
// CHECK: private unnamed_addr constant [2 x i8] c"b\00"


