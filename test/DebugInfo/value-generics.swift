// RUN: %target-swift-frontend %s -emit-ir -g -enable-builtin-module -enable-experimental-feature ValueGenerics -disable-experimental-parser-round-trip -disable-availability-checking -o - | %FileCheck %s

import Builtin

struct Vector<let N: Int, Element: ~Copyable>: ~Copyable {
  let storage: Builtin.FixedArray<N, Element>
}

extension Vector: Copyable where Element: Copyable {}

// CHECK-DAG: !DICompositeType({{.*}}name: "Builtin.FixedArray", {{.*}}identifier: "$sxq_BVD"
func genericBA<let N: Int, Element>(_: Builtin.FixedArray<N, Element>) {}

// CHECK-DAG: !DICompositeType({{.*}}name: "$s4main6VectorVyxq_GD"
func genericV<let N: Int, Element>(_: Vector<N, Element>) {}

// CHECK-DAG: !DICompositeType({{.*}}name: "Builtin.FixedArray", {{.*}}identifier: "$s$3_SiBVD"
func concreteBA(_: Builtin.FixedArray<4, Int>) {}

// CHECK-DAG: !DICompositeType({{.*}}name: "$s4main6VectorVy$1_SiGD"
func concreteV(_: Vector<2, Int>) {}
