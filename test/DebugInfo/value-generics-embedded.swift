// RUN: %target-swift-frontend %s -target %target-cpu-apple-macos14 -emit-ir -g -enable-experimental-feature ValueGenerics -enable-experimental-feature Embedded -wmo -disable-availability-checking -o - | %FileCheck %s

// REQUIR123ES: swift_feature_ValueGenerics

// CHECK-DAG: !DICompositeType({{.*}}templateParams: ![[VECTOR_PARAMS:.*]], {{.*}}identifier: "$ss6VectorVy$0_4main8MySpriteVGD"
// CHECK-DAG: ![[VECTOR_PARAMS]] = !{![[COUNT_PARAM:.*]], ![[ELEMENT_PARAM:.*]]}
// CHECK-DAG: ![[COUNT_PARAM]] = !DITemplateTypeParameter(type: ![[COUNT_TYPE:.*]])
// CHECK-DAG: ![[COUNT_TYPE]] = !DICompositeType({{.*}}name: "$s$0_D"
// CHECK-DAG: ![[ELEMENT_PARAM]] = !DITemplateTypeParameter(type: ![[ELEMENT_TYPE:.*]])
// CHECK-DAG: ![[ELEMENT_TYPE]] = !DICompositeType({{.*}}name: "MySprite", {{.*}}identifier: "$s4main8MySpriteVD"
struct MySprites {
  var bricks: Vector<1,MySprite>
}

struct MySprite {
  var x = 42
}

nonisolated(unsafe)
var sprites: MySprites? = nil
public func foo() {
    let bricks: Vector<1,MySprite> = [MySprite()]
    sprites = .init(bricks: bricks)
}
