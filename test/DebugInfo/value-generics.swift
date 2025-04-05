// RUN: %target-swift-frontend %s -emit-ir -g -enable-builtin-module -disable-availability-checking -o - | %FileCheck %s

import Builtin

struct Slab<let N: Int, Element: ~Copyable>: ~Copyable {
  let storage: Builtin.FixedArray<N, Element>
}

extension Slab: Copyable where Element: Copyable {}

// CHECK-DAG: !DICompositeType({{.*}}name: "$sxq_BVD"
func genericBA<let N: Int, Element>(_: Builtin.FixedArray<N, Element>) {}

// CHECK-DAG: !DICompositeType({{.*}}name: "$s4main4SlabVyxq_GD"
func genericV<let N: Int, Element>(_: Slab<N, Element>) {}

// CHECK-DAG: !DICompositeType({{.*}}name: "Builtin.FixedArray", {{.*}}identifier: "$s$3_SiBVD"
func concreteBA(_: Builtin.FixedArray<4, Int>) {}

// CHECK-DAG: !DICompositeType({{.*}}name: "$s4main4SlabVy$1_SiGD", {{.*}}templateParams: ![[SLAB_PARAMS:.*]])
// CHECK-DAG: ![[SLAB_PARAMS]] = !{![[COUNT_PARAM:.*]], ![[ELEMENT_PARAM:.*]]}
// CHECK-DAG: ![[COUNT_PARAM]] = !DITemplateTypeParameter(type: ![[COUNT_TYPE:.*]])
// CHECK-DAG: ![[COUNT_TYPE]] = !DICompositeType({{.*}}name: "$s$1_D"
// CHECK-DAG: ![[ELEMENT_PARAM]] = !DITemplateTypeParameter(type: ![[ELEMENT_TYPE:.*]])
// CHECK-DAG: ![[ELEMENT_TYPE]] = !DICompositeType({{.*}}"$sSiD"
func concreteV(_: Slab<2, Int>) {}
