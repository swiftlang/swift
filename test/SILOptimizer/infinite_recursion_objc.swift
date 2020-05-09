// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify

// REQUIRES: objc_interop

// A negative test that the infinite recursion pass doesn't diagnose dynamic
// dispatch.

import Foundation

class MyRecursiveClass {
  required init() {}
  @objc dynamic func foo() {
    return type(of: self).foo(self)()
  }

  @objc dynamic func foo2() {
    return self.foo()
  }
}

