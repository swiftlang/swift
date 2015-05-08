// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: OS=macosx
// FIXME: iOS doesn't have CIVector?

import QuartzCore
// Do NOT add anything that publicly imports Foundation here!

var v = CIVector(x:7);
// CHECK: x = 7
print("x = \(v.X)")
