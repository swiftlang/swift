// RUN: %target-typecheck-verify-swift -debug-cycles > %t.log 2>&1
// RUN: not grep "CYCLE DETECTED" %t.log | count 0

// Verify that extension lookups don't cause cyclic dependencies.

// expected-no-diagnostics

struct A { }
extension A { }
