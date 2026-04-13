// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s


@_marker protocol MyMarker {}

struct ConformingType: MyMarker {
}

func f() {
  let x: any MyMarker = ConformingType()
}

// Marker protocol should have the swift_marker annotation.
// CHECK-DAG: ![[MARKER:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "MyMarker"{{.*}}annotations: ![[ANNOT:[0-9]+]]
// CHECK-DAG: ![[ANNOT]] = !{![[ENTRY:[0-9]+]]}
// CHECK-DAG: ![[ENTRY]] = !{!"swift.MarkerProtocol", i1 true}
