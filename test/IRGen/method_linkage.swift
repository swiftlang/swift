// RUN: %target-swift-frontend -primary-file %s -emit-ir | FileCheck %s

// Test if all methods which go into a vtable have at least the visibility of its class.
// Reason: Derived classes from "outside" still have to put the less visible base members
// into their vtables.

class Base {
  // CHECK: define hidden void @_TFC14method_linkage4Base{{.*}}3foofS0_FT_T_
  @inline(never)
  private func foo() {
  }

  // CHECK: define internal void @_TFC14method_linkage4Base{{.*}}3barfS0_FT_T_
  @inline(never)
  private final func bar() {
  }

  // CHECK: define hidden void @_TFC14method_linkage4Base{{.*}}5otherfS0_FT_T_
  @inline(never)
  private func other() {
  }
}
class Derived : Base {
  // CHECK: define hidden void @_TFC14method_linkage7Derived{{.*}}3foofS0_FT_T_
  @inline(never)
  private final override func foo() {
  }
}

extension Base {
  // CHECK: define internal void @_TFC14method_linkage4Base{{.*}}7extfuncfS0_FT_T_
  @inline(never)
  private func extfunc() {
  }
}

public class PublicClass {
  // CHECK: define void @_TFC14method_linkage11PublicClass{{.*}}4pfoofS0_FT_T_
  @inline(never)
  private func pfoo() {
  }

  // CHECK: define void @_TFC14method_linkage11PublicClass4pbarfS0_FT_T_
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

