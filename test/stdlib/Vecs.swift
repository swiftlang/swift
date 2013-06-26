// RUN: %swift -i %s | FileCheck %s

var v1 = Vec4f(1, 2, 3, 4)
v1.replPrint(); print("\n")
// CHECK: Vec4f(1.0, 2.0, 3.0, 4.0)
var v2 = Vec4f(4, 3, 2, 1)
v2.replPrint(); print("\n")
// CHECK: Vec4f(4.0, 3.0, 2.0, 1.0)
var v3 = v1 * v2
v3.replPrint(); print("\n")
// CHECK: Vec4f(4.0, 6.0, 6.0, 4.0)

// FIXME: Bogus result?
var vb = v1 <= v2
