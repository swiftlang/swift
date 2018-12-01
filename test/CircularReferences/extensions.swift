// RUN: %target-typecheck-verify-swift -debug-cycles 2>&1 | %FileCheck --allow-empty %s

// Verify that extension lookups don't cause cyclic dependencies.

// expected-no-diagnostics

struct A { }
extension A { }

// CHECK-NOT: CYCLE DETECTED

