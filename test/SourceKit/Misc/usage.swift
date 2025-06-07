// Soundness check that -help works

// RUN: not %sourcekitd-test -help | %FileCheck %s

// CHECK: USAGE: sourcekitd-test [options] <inputs>

