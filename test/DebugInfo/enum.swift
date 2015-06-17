// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// CHECK: ![[EMPTY:.*]] = !{}
// CHECK: !DICompositeType(tag: DW_TAG_union_type, name: "Color",
// CHECK-SAME:             line: [[@LINE+3]]
// CHECK-SAME:             size: 8, align: 8,
// CHECK-SAME:             identifier: "_TtO4enum5Color"
enum Color : UInt {
// CHECK: !DIDerivedType(tag: DW_TAG_member, name: "Red"
// CHECK-SAME:           baseType: !"_TtSu"
// CHECK-SAME:           size: 8, align: 8{{[,)]}}
  case Red, Green, Blue
}

// CHECK: !DICompositeType(tag: DW_TAG_union_type, name: "MaybeIntPair",
// CHECK-SAME:             line: [[@LINE+3]],
// CHECK-SAME:             size: 136, align: 64{{[,)]}}
// CHECK-SAME:             identifier: "_TtO4enum12MaybeIntPair"
enum MaybeIntPair {
// CHECK: !DIDerivedType(tag: DW_TAG_member, name: "None"
// CHECK-SAME:           baseType: !"_TtSi"
// CHECK-SAME:           size: 136, align: 64{{[,)]}}
case None
// CHECK: !DIDerivedType(tag: DW_TAG_member, name: "Just"
// CHECK-SAME:           baseType: !"_TtTVSs5Int64S__"
// CHECK-SAME:           size: 136, align: 64{{[,)]}}
  case Just(Int64, Int64)
}

enum Maybe<T> {
  case None
  case Just(T)
}

let r = Color.Red
let c = MaybeIntPair.Just(74, 75)
// CHECK: !DICompositeType(tag: DW_TAG_union_type, name: "Maybe",
// CHECK-SAME:             line: [[@LINE-8]],
// CHECK-SAME:             size: 8, align: 8{{[,)]}}
// CHECK-SAME:             identifier: "_TtGO4enum5MaybeOS_5Color_"
let movie : Maybe<Color> = .None

public enum Nothing { }
public func foo(empty : Nothing) { }
// CHECK: !DICompositeType({{.*}}name: "Nothing", {{.*}}elements: ![[EMPTY]]

// CHECK: !DICompositeType({{.*}}name: "Rose", {{.*}}elements: ![[ELTS:[0-9]+]],
// CHECK-SAME:             {{.*}}identifier: "_TtGO4enum4Roseq__")
public enum Rose<A> {
	case MkRose(() -> A, () -> [Rose<A>])
  // CHECK: !DICompositeType({{.*}}name: "Rose", {{.*}}elements: ![[ELTS]],
  // CHECK-SAME:             {{.*}}identifier: "_TtGO4enum4RoseQq_S0__")
	case IORose(() -> Rose<A>)
}

// CHECK: !DICompositeType({{.*}}name: "Tuple", {{.*}}elements: ![[ELTS:[0-9]+]],
// CHECK-SAME:             {{.*}}identifier: "_TtGO4enum5Tupleq__")
public enum Tuple<P> {
  // CHECK: !DICompositeType({{.*}}name: "Tuple", {{.*}}elements: ![[ELTS]],
  // CHECK-SAME:             {{.*}}identifier: "_TtGO4enum5TupleQq_S0__")
	case C(P, () -> Tuple)
}
