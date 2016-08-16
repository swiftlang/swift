// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s --check-prefix=DWARF

// CHECK: ![[EMPTY:.*]] = !{}

protocol P {}

enum Either {
  case First(Int64), Second(P), Neither
// CHECK: !DICompositeType({{.*}}name: "Either",
// CHECK-SAME:             line: [[@LINE-3]],
// CHECK-SAME:             size: {{328|168}},
}
// DWARF: ![[INT:.*]] = !DICompositeType({{.*}}identifier: "_TtSi"
let E : Either = .Neither;

// CHECK: !DICompositeType({{.*}}name: "Color",
// CHECK-SAME:             line: [[@LINE+3]]
// CHECK-SAME:             size: 8, align: 8,
// CHECK-SAME:             identifier: "_TtO4enum5Color"
enum Color : UInt64 {
// This is effectively a 2-bit bitfield:
// DWARF: !DIDerivedType(tag: DW_TAG_member, name: "Red"
// DWARF-SAME:           baseType: ![[UINT64:[0-9]+]]
// DWARF-SAME:           size: 8, align: 8{{[,)]}}
// DWARF: ![[UINT64]] = !DICompositeType({{.*}}identifier: "_TtVs6UInt64"
  case Red, Green, Blue
}

// CHECK: !DICompositeType({{.*}}name: "MaybeIntPair",
// CHECK-SAME:             line: [[@LINE+3]],
// CHECK-SAME:             size: 136, align: 64{{[,)]}}
// CHECK-SAME:             identifier: "_TtO4enum12MaybeIntPair"
enum MaybeIntPair {
// DWARF: !DIDerivedType(tag: DW_TAG_member, name: "none"
// DWARF-SAME:           baseType: ![[INT]], align: 8{{[,)]}}
  case none
// DWARF: !DIDerivedType(tag: DW_TAG_member, name: "just"
// DWARF-SAME:           baseType: ![[INTTUP:[0-9]+]]
// DWARF-SAME:           size: 128, align: 64{{[,)]}}
// DWARF: ![[INTTUP]] = !DICompositeType({{.*}}identifier: "_TtTVs5Int64S__"
  case just(Int64, Int64)
}

enum Maybe<T> {
  case none
  case just(T)
}

let r = Color.Red
let c = MaybeIntPair.just(74, 75)
// CHECK: !DICompositeType({{.*}}name: "Maybe",
// CHECK-SAME:             line: [[@LINE-8]],
// CHECK-SAME:             size: 8, align: 8{{[,)]}}
// CHECK-SAME:             identifier: "_TtGO4enum5MaybeOS_5Color_"
let movie : Maybe<Color> = .none

public enum Nothing { }
public func foo(_ empty : Nothing) { }
// CHECK: !DICompositeType({{.*}}name: "Nothing", {{.*}}elements: ![[EMPTY]]

// CHECK: !DICompositeType({{.*}}name: "Rose", {{.*}}elements: ![[ELTS:[0-9]+]],
// CHECK-SAME:             {{.*}}identifier: "_TtGO4enum4Rosex_")
enum Rose<A> {
	case MkRose(() -> A, () -> [Rose<A>])
  // DWARF: !DICompositeType({{.*}}name: "Rose",{{.*}}identifier: "_TtGO4enum4RoseQq_S0__")
	case IORose(() -> Rose<A>)
}

func foo<T>(_ x : Rose<T>) -> Rose<T> { return x }

// CHECK: !DICompositeType({{.*}}name: "Tuple", {{.*}}elements: ![[ELTS:[0-9]+]],
// CHECK-SAME:             {{.*}}identifier: "_TtGO4enum5TupleQq_FS_3barurFGS0_x_GS0_x__")
// DWARF: !DICompositeType({{.*}}name: "Tuple", {{.*}}elements: ![[ELTS:[0-9]+]],
// DWARF-SAME:             {{.*}}identifier: "_TtGO4enum5Tuplex_")
public enum Tuple<P> {
  // DWARF: !DICompositeType({{.*}}name: "Tuple",{{.*}}identifier: "_TtGO4enum5TupleQq_S0__")
	case C(P, () -> Tuple)
}

func bar<T>(_ x : Tuple<T>) -> Tuple<T> { return x }

// CHECK: !DILocalVariable(name: "self", arg: 1, {{.*}} line: [[@LINE+5]], type: ![[LIST:.*]], flags: DIFlagArtificial)
// CHECK: ![[LIST]] = !DICompositeType({{.*}}identifier: "_TtGO4enum4ListQq_S0__"
public enum List<T> {
       indirect case Tail(List, T)
       case End
       func fooMyList() {}
}
