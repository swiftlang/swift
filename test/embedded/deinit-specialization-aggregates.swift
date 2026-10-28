// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ValueGenerics -enable-experimental-feature MoveOnlyTuples -Osize -emit-sil -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ValueGenerics -enable-experimental-feature MoveOnlyTuples -Onone -c -o /dev/null

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_ValueGenerics
// REQUIRES: swift_feature_MoveOnlyTuples

// CHECK-DAG: sil_moveonlydeinit $Element<Int> {

public struct Element<T>: ~Copyable {
  var p: UnsafeMutablePointer<Int>
  public init() { p = .allocate(capacity: 1) }
  deinit { p.deallocate() }
}

// An InlineArray of the generic type, inside a type whose metadata is emitted
// eagerly.
@export(interface)
public struct ArrayBox: ~Copyable {
  var a: InlineArray<2, Element<Int>>
  public init() { a = InlineArray<2, Element<Int>>(first: Element<Int>()) { _ in Element<Int>() } }
}

// A tuple containing the generic type.
@export(interface)
public struct TupleBox: ~Copyable {
  var t: (Element<Int>, Int)
  public init() { t = (Element<Int>(), 1) }
}

// A tuple nested inside an InlineArray, to check the recursion composes.
@export(interface)
public struct NestedBox: ~Copyable {
  var a: InlineArray<2, (Element<Int>, Int)>
  public init() {
    a = InlineArray<2, (Element<Int>, Int)>(first: (Element<Int>(), 0)) { _ in (Element<Int>(), 1) }
  }
}

public func use() {
  _ = ArrayBox()
  _ = TupleBox()
  _ = NestedBox()
}
