// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

class GenericObjCSubclass<T>: NSObject {
  var genericValue: T

  init(_ t: T) {
    self.genericValue = t
    super.init()
  }
}

func f() {
  let variable = GenericObjCSubclass<Int>(32)
}

// CHECK-DAG: ![[GENERIC_OBJC_SUBCLASS:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "GenericObjCSubclass", {{.*}}identifier: "$s30generic_class_objc_inheritance19GenericObjCSubclassCyxGD")
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[GENERIC_OBJC_SUBCLASS]], baseType: ![[NSOBJECT:[0-9]+]]
// CHECK-DAG: ![[NSOBJECT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "NSObject"
