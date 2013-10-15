// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -i %s | FileCheck %s
// REQUIRES: sdk

import QuartzCore
// Do NOT add anything that publicly imports Foundation here!

var v = CIVector(withX:7);
// CHECK: x = 7
println("x = \(v.X())");
