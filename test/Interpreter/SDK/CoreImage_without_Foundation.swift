// RUN: %target-run-simple-swift | FileCheck %s

// UNSUPPORTED: linux

import CoreImage
// Do NOT add anything that publicly imports Foundation here!

var v = CIVector(x:7);
// CHECK: x = 7
print("x = \(v.X)")
