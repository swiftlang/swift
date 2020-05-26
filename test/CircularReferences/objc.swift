// RUN: %target-typecheck-verify-swift -debug-cycles > %t.log 2>&1
// RUN: not grep "CYCLE DETECTED" %t.log | count 0

// REQUIRES: objc_interop

// Verify that isObjC computation doesn't cause cyclic dependencies.

// expected-no-diagnostics

class A {
  @objc func foo() { }
}


@objc class B {
  @objc dynamic subscript(i: Int) -> B {
    return self
  }
}

class C: B {
  override subscript(i: Int) -> B {
    return super[i]
  }
}
