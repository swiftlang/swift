// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %target-swift-emit-silgen -enable-experimental-feature ObjCDirect %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -enable-experimental-feature ObjCDirect %s | %FileCheck %s --check-prefix=WITNESS

import Foundation

// MARK: Dispatch

// Swift-side dispatch for a direct member that is *not* written 'final'. A
// direct member has no vtable slot, so every reference to it is static.

class NonFinalClass: NSObject {
  @objcDirect func directMethod() -> Int { 1 }

  @objcDirect var directProperty: Int = 0

  @objcDirect subscript(i: Int) -> Int { i }

  // Control, so the absences below can't pass on empty output.
  @objc func normalMethod() -> Int { 2 }
}

// CHECK-LABEL: sil {{.*}}callDirectMethod
// CHECK-NOT: class_method
// CHECK: } // end sil function
func callDirectMethod(_ c: NonFinalClass) -> Int {
  return c.directMethod()
}

// CHECK-LABEL: sil {{.*}}callDirectProperty
// CHECK-NOT: class_method
// CHECK: } // end sil function
func callDirectProperty(_ c: NonFinalClass) -> Int {
  c.directProperty = 1
  return c.directProperty
}

// CHECK-LABEL: sil {{.*}}callDirectSubscript
// CHECK-NOT: class_method
// CHECK: } // end sil function
func callDirectSubscript(_ c: NonFinalClass) -> Int {
  return c[0]
}

// CHECK-LABEL: sil {{.*}}callNormalMethod
// CHECK: class_method
// CHECK: } // end sil function
func callNormalMethod(_ c: NonFinalClass) -> Int {
  return c.normalMethod()
}

class Subclass: NonFinalClass {
  // CHECK-LABEL: sil {{.*}}Subclass{{.*}}callSuper
  // CHECK-NOT: super_method
  // CHECK: } // end sil function
  func callSuper() -> Int {
    return super.directMethod()
  }
}

protocol HasDirectRequirements {
  func directMethod() -> Int
  var directProperty: Int { get set }
  subscript(i: Int) -> Int { get }
}

extension NonFinalClass: HasDirectRequirements {}

// WITNESS-LABEL: // protocol witness for HasDirectRequirements.directMethod()
// WITNESS-NOT: class_method
// WITNESS: } // end sil function
// WITNESS-LABEL: // protocol witness for HasDirectRequirements.directProperty.getter
// WITNESS-NOT: class_method
// WITNESS: } // end sil function
// WITNESS-LABEL: // protocol witness for HasDirectRequirements.directProperty.setter
// WITNESS-NOT: class_method
// WITNESS: } // end sil function
// WITNESS-LABEL: // protocol witness for HasDirectRequirements.directProperty.modify
// WITNESS-NOT: class_method
// WITNESS: } // end sil function
// WITNESS-LABEL: // protocol witness for HasDirectRequirements.subscript.getter
// WITNESS-NOT: class_method
// WITNESS: } // end sil function

// MARK: Key paths

// A key path to a direct property carries no KVC string.

class C: NSObject {
  @objcDirect var direct: Int = 0
  @objc var normal: Int = 0
}

// CHECK-LABEL: // directKeyPath()
// CHECK: keypath ${{.*}}, (root $C;
// CHECK: } // end sil function
func directKeyPath() -> AnyKeyPath { \C.direct }

// CHECK-LABEL: // normalKeyPath()
// CHECK: keypath ${{.*}}, (objc "normal"; root $C;
// CHECK: } // end sil function
func normalKeyPath() -> AnyKeyPath { \C.normal }
