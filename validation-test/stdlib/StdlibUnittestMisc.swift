// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out
// RUN: %target-run %t.out | FileCheck %s

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

