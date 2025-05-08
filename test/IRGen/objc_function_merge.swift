// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O %s -module-name=test -o %t/a.out
// RUN: %target-build-swift -O %s -module-name=test -emit-ir | %FileCheck --check-prefix=CHECK-IR %s
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

// In some places Foundation is comparing ObjC method pointers.
// Therefore LLVM's function merging pass must not create aliases for identical
// functions, but create thunks. This can be ensured if ObjC methods are not
// created with the unnamed_addr attribute.

import Foundation

class Base: NSObject, NSSecureCoding {
  @objc public class var supportsSecureCoding: Bool {
    return true
  }
  
  @objc let s: String
  
  func encode(with coder: NSCoder) {
    coder.encode(s, forKey:  #keyPath(s))
  }
  
  init(s: String) {
    self.s = s
  }
  
  required init?(coder: NSCoder) {
    self.s = coder.value(forKey: #keyPath(s)) as! String
  }
}

class Derived : Base {
  // Make sure the overridden method is not merged with the base method (without
  // creating a thunk), so that the method pointers remain distinct.
  @objc public class override var supportsSecureCoding: Bool {
    return true
  }
}


// Check if the objc methods are not generated with the unnamed_addr attribute.
// CHECK-IR-DAG: define {{.*}} @"$s4test4BaseC20supportsSecureCodingSbvgZTo"({{.*}}) #{{[0-9]+}} {
// CHECK-IR-DAG: define {{.*}} @"$s4test4BaseC6encode4withySo7NSCoderC_tFTo"({{.*}}) #{{[0-9]+}} {
// CHECK-IR-DAG: define {{.*}} @"$s4test7DerivedC20supportsSecureCodingSbvgZTo"({{.*}}) #{{[0-9]+}} {

let d = Derived(s: "")
if #available(macOS 10.13, iOS 11, tvOS 11, watchOS 4, *) {
  // Check that we don't crash here.
  _ = try NSKeyedArchiver.archivedData(withRootObject: d, requiringSecureCoding: true)
}
// CHECK: okay
print("okay")
