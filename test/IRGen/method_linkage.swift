// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// Test if all methods which go into a vtable have at least the visibility of its class.
// Reason: Derived classes from "outside" still have to put the less visible base members
// into their vtables.

class Base {
  // CHECK: define hidden void @_T014method_linkage4Base{{.*}}3foo0
  @inline(never)
  fileprivate func foo() {
  }

  // CHECK: define internal void @_T014method_linkage4Base{{.*}}3bar0
  @inline(never)
  fileprivate final func bar() {
  }

  // CHECK: define hidden void @_T014method_linkage4Base{{.*}}5other0
  @inline(never)
  fileprivate func other() {
  }
}
class Derived : Base {
  // CHECK: define hidden void @_T014method_linkage7Derived{{.*}}3foo0
  @inline(never)
  fileprivate final override func foo() {
  }
}

extension Base {
  // CHECK: define internal void @_T014method_linkage4Base{{.*}}7extfunc0
  @inline(never)
  fileprivate func extfunc() {
  }
}

public class PublicClass {
  // CHECK: define{{( protected)?}} void @_T014method_linkage11PublicClass{{.*}}4pfoo0
  @inline(never)
  fileprivate func pfoo() {
  }

  // CHECK: define{{( protected)?}} void @_T014method_linkage11PublicClassC4pbaryyF
  @inline(never)
  internal func pbar() {
  }
}

// Just in case anyone wants to delete unused methods...
func callit(b: Base, p: PublicClass) {
  b.foo()
  b.bar()
  b.other()
  b.extfunc()
  p.pfoo()
  p.pbar()
}

