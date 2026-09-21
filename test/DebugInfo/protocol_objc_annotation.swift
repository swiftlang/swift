// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

@objc protocol ObjCBound {}

// An @objc protocol has no witness table, which the debugger needs to know to
// lay out an existential of it.
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "ObjCBound"{{.*}}annotations: ![[ANNOT:[0-9]+]]
// CHECK-DAG: ![[ANNOT]] = !{![[ENTRY:[0-9]+]]}
// CHECK-DAG: ![[ENTRY]] = !{!"swift.ObjCProtocol", i1 true}

class C: NSObject, ObjCBound {}

func f() {
  let objcBound: any ObjCBound = C()
}
