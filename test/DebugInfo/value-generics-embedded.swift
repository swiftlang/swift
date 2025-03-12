// RUN: %target-swift-frontend %s -target %target-cpu-apple-macos14 -emit-ir -g -enable-experimental-feature Embedded -wmo -disable-availability-checking -o - | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib

// REQUIRES: swift_feature_Embedded

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "InlineArray",{{.*}}size: 64{{.*}}elements: ![[ELTS:[0-9]+]], runtimeLang: DW_LANG_Swift, templateParams: ![[SLAB_PARAMS:[0-9]+]], identifier: "$es11InlineArrayVy$0_4main8MySpriteVGD", specification:
// CHECK-DAG: ![[SLAB_PARAMS]] = !{![[COUNT_PARAM:.*]], ![[ELEMENT_PARAM:.*]]}
// CHECK-DAG: ![[COUNT_PARAM]] = !DITemplateTypeParameter(type: ![[COUNT_TYPE:.*]])
// CHECK-DAG: ![[COUNT_TYPE]] = !DICompositeType({{.*}}name: "$e$0_D"
// CHECK-DAG: ![[ELEMENT_PARAM]] = !DITemplateTypeParameter(type: ![[ELEMENT_TYPE:.*]])
// CHECK-DAG: ![[ELEMENT_TYPE]] = !DICompositeType({{.*}}name: "MySprite", {{.*}}identifier: "$e4main8MySpriteVD"

// CHECK-DAG: ![[ELTS]] = !{![[STORAGE_MEMBER:.*]]}
// CHECK-DAG: ![[STORAGE_MEMBER]] = !DIDerivedType(tag: DW_TAG_member, name: "_storage",{{.*}}baseType: ![[ARRAY:[0-9]+]], size: 64
// CHECK-DAG: ![[ARRAY]] = !DICompositeType(tag: DW_TAG_array_type, baseType: ![[ELEMENT_TYPE]], size: 64, elements: ![[SUBRANGES:[0-9]+]])
// CHECK-DAG: ![[SUBRANGES]] = !{![[DIM:[0-9]+]]}
// CHECK-DAG: ![[DIM]] = !DISubrange(count: 1)

struct MySprites {
  var bricks: InlineArray<1, MySprite>
}

struct MySprite {
  var x = 42
}

nonisolated(unsafe)
var sprites: MySprites? = nil
public func foo() {
    let bricks: InlineArray<1, MySprite> = [MySprite()]
    sprites = .init(bricks: bricks)
}
