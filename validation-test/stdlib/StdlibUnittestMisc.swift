// RUN: %target-run-simple-swift | FileCheck %s

import StdlibUnittest

//
// Test OS version parsing
//

// CHECK: (10, 0, 0)
print(_parseDottedVersionTriple("10"))

// CHECK: (10, 9, 0)
print(_parseDottedVersionTriple("10.9"))

// CHECK: (10, 9, 3)
print(_parseDottedVersionTriple("10.9.3"))

