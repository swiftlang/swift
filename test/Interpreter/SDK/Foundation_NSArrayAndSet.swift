// RUN: %target-run-simple-swift | FileCheck %s
// rdar://16726444

import Foundation

var MyArray = NSArray(objects:"A", "B", "C", "D")
print(MyArray)
// CHECK: (
// CHECK: A,
// CHECK: B,
// CHECK: C,
// CHECK: D
// CHECK: )

var MySet = NSSet(objects:"a", "b", "c", "42")
MySet = NSSet(objects:"a", "b", "c", "42")
print(MySet)
// CHECK:{(
// CHECK:    b,
// CHECK:    42,
// CHECK:    c,
// CHECK:    a
// CHECK:)}
