// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s

// Test if all methods which go into a vtable have at least the visibility of its class.
// Reason: Derived classes from "outside" still have to put the less visible base members
// into their vtables.

class Base {
  // CHECK: define hidden void @_TFC14method_linkage4Base{{.*}}3foofT_T_
  @inline(never)
  fileprivate func foo() {
  }

  // CHECK: define internal void @_TFC14method_linkage4Base{{.*}}3barfT_T_
  @inline(never)
  fileprivate final func bar() {
  }

  // CHECK: define hidden void @_TFC14method_linkage4Base{{.*}}5otherfT_T_
  @inline(never)
  fileprivate func other() {
  }
}
class Derived : Base {
  // CHECK: define hidden void @_TFC14method_linkage7Derived{{.*}}3foofT_T_
  @inline(never)
  fileprivate final override func foo() {
  }
}

extension Base {
  // CHECK: define internal void @_TFC14method_linkage4Base{{.*}}7extfuncfT_T_
  @inline(never)
  fileprivate func extfunc() {
  }
}

public class PublicClass {
  // CHECK: define{{( protected)?}} void @_TFC14method_linkage11PublicClass{{.*}}4pfoofT_T_
  @inline(never)
  fileprivate func pfoo() {
  }

  // CHECK: define{{( protected)?}} void @_TFC14method_linkage11PublicClass4pbarfT_T_
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

