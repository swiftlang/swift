// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import QuartzCore
// Do NOT add anything that publicly imports Foundation here!

var v = CIVector(x:7);
// CHECK: x = 7
print("x = \(v.X)")
