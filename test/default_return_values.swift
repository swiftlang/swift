// RUN: %swift %s -i | FileCheck %s
// XFAIL: *
func f() -> (result : Int = 12345) {}
println(f() - 300)
// CHECK: 12045
