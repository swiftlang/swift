// RUN: %target-run-simple-swift | FileCheck %s

// XFAIL: linux

import StdlibUnittest

//
// Test OS version parsing
//

// CHECK: (10, 0, 0)
println(_parseDottedVersionTriple("10"))

// CHECK: (10, 9, 0)
println(_parseDottedVersionTriple("10.9"))

// CHECK: (10, 9, 3)
println(_parseDottedVersionTriple("10.9.3"))

