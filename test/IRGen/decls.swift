// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// Multiple local decls.
func test1() {
  class a { var x, y : Int }
  class b { var a, b : Int }
  var _ : a, _ : b
}

// CHECK: private unnamed_addr constant [2 x i8] c"a\00"
// CHECK: private unnamed_addr constant [2 x i8] c"b\00"


