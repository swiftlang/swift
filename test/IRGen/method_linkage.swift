// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s

// Test if all methods which go into a vtable have at least the visibility of its class.
// Reason: Derived classes from "outside" still have to put the less visible base members
// into their vtables.

// Method descriptors linkage:

// - internal initializer descriptor has hidden linkage when class is public:
// CHECK-LABEL: @"$s14method_linkage11PublicClassCACycfCTq" = hidden alias

// - internal initializer descriptor has public linkage when class is open:
// CHECK-LABEL: @"$s14method_linkage9OpenClassCACycfCTq" ={{( dllexport)?}}{{( protected)?}} alias

// - private method descriptor has internal linkage even though class is open:
// CHECK: @"$s14method_linkage9OpenClassC4pfoo0{{.*}}FTq" = internal alias

class Base {
  // CHECK: define hidden swiftcc void @"$s14method_linkage4Base{{.*}}3foo0
  @inline(never)
  fileprivate func foo() {
  }

  // CHECK: define internal swiftcc void @"$s14method_linkage4Base{{.*}}3bar0
  @inline(never)
  fileprivate final func bar() {
  }

  // CHECK: define hidden swiftcc void @"$s14method_linkage4Base{{.*}}5other0
  @inline(never)
  fileprivate func other() {
  }
}
class Derived : Base {
  // CHECK: define internal swiftcc void @"$s14method_linkage7Derived{{.*}}3foo0
  @inline(never)
  fileprivate final override func foo() {
  }
}

extension Base {
  // CHECK: define internal swiftcc void @"$s14method_linkage4Base{{.*}}7extfunc0
  @inline(never)
  fileprivate func extfunc() {
  }
}

public class PublicClass {
  internal init() {}

  // CHECK: define hidden swiftcc void @"$s14method_linkage11PublicClass{{.*}}4pfoo0
  @inline(never)
  fileprivate func pfoo() {
  }

  // CHECK: define hidden swiftcc void @"$s14method_linkage11PublicClassC4pbaryyF
  @inline(never)
  internal func pbar() {
  }
}

open class OpenClass {
  internal init() {}

  // CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage9OpenClassC4pfoo0
  @inline(never)
  fileprivate func pfoo() {
  }

  // CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage9OpenClassC4pbaryyF
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

