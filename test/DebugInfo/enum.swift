// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

// UNSUPPORTED: OS=watchos

protocol P {}

enum Either {
  case First(Int64), Second(P), Neither
// CHECK: !DICompositeType({{.*}}name: "Either",
// CHECK-SAME:             line: [[@LINE-3]],
// CHECK-SAME:             size: {{328|168}},
}
// CHECK: ![[EMPTY:.*]] = !{}
let E : Either = .Neither;

// CHECK: !DICompositeType({{.*}}name: "Color",
// CHECK-SAME:             line: [[@LINE+3]]
// CHECK-SAME:             size: 8,
// CHECK-SAME:             identifier: "$s4enum5ColorOD"
enum Color : UInt64 {
  case Red, Green, Blue
}

// CHECK: !DICompositeType({{.*}}name: "MaybeIntPair",
// CHECK-SAME:             line: [[@LINE+3]],
// CHECK-SAME:             size: 136{{[,)]}}
// CHECK-SAME:             identifier: "$s4enum12MaybeIntPairOD"
enum MaybeIntPair {
  case none
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
// CHECK-SAME:             size: 8{{[,)]}}
// CHECK-SAME:             identifier: "$s4enum5MaybeOyAA5ColorOGD"
let movie : Maybe<Color> = .none

public enum Nothing { }
public func foo(_ empty : Nothing) { }
// CHECK: !DICompositeType({{.*}}name: "Nothing", {{.*}}elements: ![[EMPTY]]

// CHECK: !DICompositeType({{.*}}name: "Rose", {{.*}}elements: ![[ELTS:[0-9]+]],
// CHECK-SAME:             {{.*}}identifier: "$s4enum4RoseOyxG{{z?}}D")
enum Rose<A> {
	case MkRose(() -> A, () -> [Rose<A>])
  // DWARF: !DICompositeType({{.*}}name: "Rose",{{.*}}identifier: "$s4enum4RoseOyxGD")
	case IORose(() -> Rose<A>)
}

func foo<T>(_ x : Rose<T>) -> Rose<T> { return x }

// CHECK: !DICompositeType({{.*}}name: "Tuple", {{.*}}elements: ![[ELTS:[0-9]+]], {{.*}}identifier: "$s4enum5TupleOyxGD")
public enum Tuple<P> {
	case C(P, () -> Tuple)
}

func bar<T>(_ x : Tuple<T>) -> Tuple<T> { return x }

// CHECK: ![[LIST:.*]] = !DICompositeType({{.*}}identifier: "$s4enum4ListOyxGD"
// CHECK: !DILocalVariable(name: "self", arg: 1, {{.*}} line: [[@LINE+4]], type: ![[LIST]], flags: DIFlagArtificial)
public enum List<T> {
       indirect case Tail(List, T)
       case End
       func fooMyList() {}
}
