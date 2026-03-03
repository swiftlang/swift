// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s
// CHECK: !DICompositeType(tag: DW_TAG_variant_part, {{.*}}elements: ![[ELTS:[0-9]+]])
// CHECK: ![[ELTS]] = !{![[ML:[0-9]+]], ![[MR:[0-9]+]]}
// CHECK: ![[ML]] = !DIDerivedType(tag: DW_TAG_member, name: "left",
// CHECK: ![[MR]] = !DIDerivedType(tag: DW_TAG_member, name: "right",
enum Either<Left, Right> {
  case left(Left)
  case right(Right)
}

let either = Either<Int, Double>.left(1234)
