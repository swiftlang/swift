// RUN: %swift -sdk=%sdk -constraint-checker -i %s | FileCheck %s
// REQUIRES: sdk

import Foundation

var a : NSArray = ["one", 2, [1,2,3]]

// CHECK: one
// CHECK: 2
// CHECK: (
// CHECK:   1,
// CHECK:   2,
// CHECK:   3
// CHECK: )

for x in a {
  println(x.description())
}
