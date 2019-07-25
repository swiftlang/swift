// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name=_Test -import-objc-header %S/Inputs/check_class_for_archiving.h -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

class SwiftClass {}

class ObjcClass : NSObject {}

private class PrivateClass : NSObject {}

@objc(named_class)
private class NamedClass1 : NSObject {}

@objc(_T3nix11NamedClass2C)
private class NamedClass2 : NSObject {}

class GenericClass<T> : NSObject {}

class DerivedClass : GenericClass<Int> {}

@objc(_T3nix20DerivedClassWithNameC)
private class DerivedClassWithName : GenericClass<Int> {}

struct ABC {
  class InnerClass : NSObject {}
}

struct DEF<T> {
  class InnerClass : NSObject {}
}

let op: Int32 = 0 // archiving

// CHECK: SwiftClass: 0
print("SwiftClass: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(SwiftClass.self, operation: op))")
// CHECK: ObjcClass: 0
print("ObjcClass: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(ObjcClass.self, operation: op))")
// CHECK: NamedClass1: 0
print("NamedClass1: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(NamedClass1.self, operation: op))")
// CHECK: NamedClass2: 0
print("NamedClass2: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(NamedClass2.self, operation: op))")
// CHECK: DerivedClass: 0
print("DerivedClass: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(DerivedClass.self, operation: op))")
// CHECK: DerivedClassWithName: 0
print("DerivedClassWithName: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(DerivedClass.self, operation: op))")
// CHECK: NSKeyedUnarchiver: 0
print("NSKeyedUnarchiver: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(NSKeyedUnarchiver.self, operation: op))")

if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  // CHECK: PrivateClass: 2
  print("PrivateClass: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(PrivateClass.self, operation: op))")
  // CHECK: GenericClass: 1
  print("GenericClass: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(GenericClass<Int>.self, operation: op))")
  // CHECK: InnerClass: 2
  print("InnerClass: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(ABC.InnerClass.self, operation: op))")
  // CHECK: InnerClass2: 1
  print("InnerClass2: \(NSKeyedUnarchiver._swift_checkClassAndWarnForKeyedArchiving(DEF<Int>.InnerClass.self, operation: op))")
} else {
  // Disable the checks for older OSes because of rdar://problem/50504765
  print("PrivateClass: 2")
  print("GenericClass: 1")
  print("InnerClass: 2")
  print("InnerClass2: 1")
}
