// RUN: not %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded 2>&1 | %FileCheck %s

// CHECK: error: Whole module optimization (wmo) must be enabled with embedded Swift.
