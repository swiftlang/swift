// RUN: not %target-swift-autolink-extract 2>&1 | %FileCheck %s

// REQUIRES: autolink-extract

// CHECK: <unknown>:0: error: this mode requires at least one input file
