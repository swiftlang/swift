// RUN: %target-swift-frontend -emit-ir -Onone %s | %FileCheck %s

// https://github.com/swiftlang/swift/issues/91477
// The first captured key path argument's offset must be rounded up to its
// own alignment even when a generic-requirements block precedes it.

struct Arg<Value>: Hashable {
  static func == (l: Self, r: Self) -> Bool { true }
  func hash(into h: inout Hasher) {}
  let value: Value
  init(_ v: Value) { value = v }
}

struct Root {
  subscript<V>(meta arg: Arg<V>) -> Int { get { 0 } set { } }
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}4makeys10AnyKeyPathCxlF"
// Round the operand's offset up to its own alignment: (ptrSize + mask) & ~mask.
// (Integer width and the ptrSize constant vary with the target's pointer size.)
// CHECK: %flags.alignmentMask = and i{{32|64}} %{{.*}}, 255
// CHECK: %[[NOT_MASK:[0-9]+]] = xor i{{32|64}} %flags.alignmentMask, -1
// CHECK: %[[SUM:[0-9]+]] = add i{{32|64}} {{4|8}}, %flags.alignmentMask
// CHECK: %[[ROUNDED_OFFSET:[0-9]+]] = and i{{32|64}} %[[SUM]], %[[NOT_MASK]]
// ...and store the operand at that rounded offset, not the raw sum above.
// CHECK: getelementptr inbounds i8, ptr %{{[0-9]+}}, i{{32|64}} %[[ROUNDED_OFFSET]]
// Bounds the search above to `make`'s body
// CHECK-LABEL: define {{.*}} @"$s{{.*}}3ArgVyxGlTH"
func make<V>(_ v: V) -> AnyKeyPath {
  let arg = Arg(v)
  return \Root[meta: arg]
}
