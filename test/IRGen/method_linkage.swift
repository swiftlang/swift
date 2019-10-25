// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-library-evolution | %FileCheck %s --check-prefix=RESILIENT

// Test if all methods which go into a vtable have at least the visibility of its class.
// Reason: Derived classes from "outside" still have to put the less visible base members
// into their vtables.

// Method descriptors linkage:

// - internal initializer descriptor has hidden linkage when class is public:
// CHECK-LABEL: @"$s14method_linkage11PublicClassCACycfCTq" = hidden alias

// - internal initializer descriptor has hidden linkage when class is open:
// CHECK-LABEL: @"$s14method_linkage9OpenClassCACycfCTq" = hidden alias

// - private method descriptor has internal linkage even though class is open:
// CHECK: @"$s14method_linkage9OpenClassC4pfoo0{{.*}}FTq" = internal alias

class Base {
  // CHECK: define hidden swiftcc void @"$s14method_linkage4Base{{.*}}3foo0
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage4Base{{.*}}3foo0
  fileprivate func foo() {
  }

  // CHECK: define internal swiftcc void @"$s14method_linkage4Base{{.*}}3bar0
  // RESILIENT: define internal swiftcc void @"$s14method_linkage4Base{{.*}}3bar0
  fileprivate final func bar() {
  }

  // CHECK: define hidden swiftcc void @"$s14method_linkage4Base{{.*}}5other0
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage4Base{{.*}}5other0
  fileprivate func other() {
  }

  // CHECK: define hidden swiftcc void @"$s14method_linkage4BaseC4prop{{.*}}LLytvg
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage4BaseC4prop{{.*}}LLytvg
  fileprivate var prop: () {
    return ()
  }
}
class Derived : Base {
  // CHECK: define hidden swiftcc void @"$s14method_linkage7Derived{{.*}}3foo0
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage7Derived{{.*}}3foo0
  fileprivate final override func foo() {
  }

  // CHECK: define hidden swiftcc void @"$s14method_linkage7DerivedC4prop{{.*}}LLytvg
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage7DerivedC4prop{{.*}}LLytvg
  fileprivate final override var prop: () {
    return ()
  }
}

extension Base {
  // CHECK: define internal swiftcc void @"$s14method_linkage4Base{{.*}}7extfunc0
  // RESILIENT: define internal swiftcc void @"$s14method_linkage4Base{{.*}}7extfunc0
  fileprivate func extfunc() {
  }
}

public class PublicClass {
  internal init() {}

  // CHECK: define hidden swiftcc void @"$s14method_linkage11PublicClass{{.*}}4pfoo0
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage11PublicClass{{.*}}4pfoo0
  fileprivate func pfoo() {
  }

  // CHECK: define hidden swiftcc void @"$s14method_linkage11PublicClassC4pbaryyF
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage11PublicClassC4pbaryyF
  internal func pbar() {
  }

  // CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage11PublicClassC4pbazyyF"
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage11PublicClassC4pbazyyF"
  public func pbaz() {
  }

  // CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage11PublicClassC5pquuxyyF"
  // RESILIENT: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage11PublicClassC5pquuxyyF"
  public final func pquux() {
  }
}

open class OpenClass {
  internal init() {}

  // CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage9OpenClassC4pfoo0
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage9OpenClassC4pfoo0
  fileprivate func pfoo() {
  }

  // CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage9OpenClassC4pbaryyF
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage9OpenClassC4pbaryyF
  internal func pbar() {
  }

  // CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage9OpenClassC4pbazyyF"
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage9OpenClassC4pbazyyF"
  public func pbaz() {
  }

  // CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage9OpenClassC5pquuxyyF"
  // RESILIENT: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage9OpenClassC5pquuxyyF"
  public final func pquux() {
  }

  // CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage9OpenClassC4prop{{.*}}LLytvg
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage9OpenClassC4prop{{.*}}LLytvg
  fileprivate var prop: () {
    return ()
  }
}

open class OpenSubclass : OpenClass {
  // CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s14method_linkage12OpenSubclassC4prop{{.*}}LLytvg
  // RESILIENT: define hidden swiftcc void @"$s14method_linkage12OpenSubclassC4prop{{.*}}LLytvg
  fileprivate final override var prop: () {
    return ()
  }
}

// Just in case anyone wants to delete unused methods...
func callit(b: Base) {
  b.foo()
  b.bar()
  b.other()
  b.extfunc()
  _ = b.prop
}
