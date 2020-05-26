// RUN: %target-typecheck-verify-swift -debug-cycles 2>&1 | %FileCheck --allow-empty %s

// Verify that protocol where clause lookups don't cause cyclic dependencies.

// expected-no-diagnostics

class C { }
protocol Q { }
protocol P where Self : Q, Self : C { }

// CHECK-NOT: CYCLE DETECTED

