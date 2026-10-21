// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// '@objcDirect' implies '@objc' however early IsObjCRequest is evaluated.

// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature ObjCDirect %s | %FileCheck %s

import Foundation

// Checking the subclass first walks the superclass's members before the
// superclass's own attributes have been checked.
public class Sub: Base {}

public class Base: NSObject {
  // CHECK-DAG: define {{.*}}@"-[Base foo]D"(ptr %0)
  @objcDirect public func foo() -> Int { 1 }
}

public class C: NSObject {}

extension C {
  // The access modifier makes visitAccessControlAttr ask isObjC() before the
  // @objcDirect attribute is visited.
  // CHECK-DAG: define {{.*}}@"-[C directOnly]D"(ptr %0)
  @objcDirect public func directOnly() -> Int { 1 }
}
