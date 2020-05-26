// RUN: %target-swift-frontend -emit-ir -primary-file %s -module-name A | %FileCheck %s

// REQUIRES: CPU=armv7k
// REQUIRES: OS=watchos

import Foundation

func getAClass(managedObjectClass : AnyClass) -> AnyClass{
  let metaClass: AnyClass = object_getClass(managedObjectClass)!
  return metaClass
}

public class ObjCSubclass : NSObject {
  public final var field: Int32 = 0
}

func test(_ o: ObjCSubclass) {
  o.field = 10
}

// CHECK: declare i8* @object_getClass(i8*)
// CHECK: call %objc_class* bitcast (i8* (i8*)* @object_getClass to %objc_class* (%objc_object*)*)(%objc_object* %{{.*}})
