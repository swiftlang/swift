// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

class Base {}

protocol Opaque {}
protocol AnyObjectBound: AnyObject {}
protocol SuperclassBound: Base {}

class C: Opaque, AnyObjectBound {}
class D: Base, SuperclassBound {}

func f() {
  let opaque: any Opaque = C()
  let anyObjectBound: any AnyObjectBound = C()
  let superclassBound: any SuperclassBound = D()
}

// Both a class constraint spelled as AnyObject and one implied by a superclass
// requirement are annotated, and share the annotation.
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "AnyObjectBound"{{.*}}annotations: ![[ANNOT:[0-9]+]]
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "SuperclassBound"{{.*}}annotations: ![[ANNOT]]
// CHECK-DAG: ![[ANNOT]] = !{![[ENTRY:[0-9]+]]}
// CHECK-DAG: ![[ENTRY]] = !{!"swift.ClassConstrainedProtocol", i1 true}

// An unconstrained protocol carries no annotation, which is what gives it the
// plain Protocol field descriptor kind.
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Opaque"{{.*}}identifier: "{{.*}}6Opaque_pD")
