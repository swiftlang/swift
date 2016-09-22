// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

public typealias Pair<T> = (T, T)
public let range : Pair<Int> = (1, 2)
public struct Outer<P> {
  public typealias FunTy<Q> = (Q) -> (P)
  func getIntFn() -> FunTy<Int>? { return nil }
}
public let fn = Outer<Double>().getIntFn()

public func fst<U>(x: Pair<U>) -> U {
  return x.0
}

// CHECK: DIGlobalVariable({{.*}}name: "range"{{.*}}, type: ![[PAIR:[0-9]+]]
// CHECK: ![[PAIR]] = !DICompositeType(tag: DW_TAG_structure_type,
// CHECK-SAME:                         name: "Pair"
// CHECK-SAME:                         line: 3
// CHECK-SAME:                         identifier: "_TtGa4main4PairSi_"

// CHECK: DIGlobalVariable({{.*}}name: "fn"{{.*}}, type: ![[OPTFN:[0-9]+]]
// CHECK: ![[OPTFN]] = !DICompositeType(tag: DW_TAG_structure_type,
// CHECK-SAME:            identifier: "_TtGSqGaV4main5Outer5FunTySd_Si__"

// CHECK: DILocalVariable({{.*}}name: "x"{{.*}}, type: ![[GEN:[0-9]+]]
// CHECK: ![[GEN]] = !DICompositeType(tag: DW_TAG_structure_type,
// CHECK-SAME:                        name: "Pair"
// CHECK-SAME:            identifier: "_TtGa4main4PairQq_FS_3fsturFT1xTxx__x_"
