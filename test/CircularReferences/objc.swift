// RUN: %target-typecheck-verify-swift -debug-cycles > %t.log 2>&1
// RUN: not grep "CYCLE DETECTED" %t.log | count 0

// REQUIRES: objc_interop

// Verify that isObjC computation doesn't cause cyclic dependencies.

// expected-no-diagnostics

class A {
  @objc func foo() { }
}
