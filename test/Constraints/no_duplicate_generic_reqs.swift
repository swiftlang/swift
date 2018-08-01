// RUN: %target-typecheck-verify-swift -debug-constraints %s > %t.dump 2>&1
// RUN: %FileCheck %s < %t.dump

struct B<T: Equatable> {
  static func foo(_: [T]) {}
}

// CHECK: $T2 equivalent to $T0
// CHECK-NOT: $T0 conforms to Equatable
// CHECK: $T2 conforms to Equatable
B.foo([42])


struct P {
  var id: Int64
}

struct List<T: Collection, E: Hashable> {
  typealias Data = T.Element
  init(_: T, id: KeyPath<Data, E>) {}
}

var arr: [P] = []

// CHECK: $T10 equivalent to $T1
// CHECK: $T11 equivalent to $T2
// CHECK-NOT: $T1 conforms to Collection
// CHECK: $T10 conforms to Collection
// CHECK-NOT: $T2 conforms to Hashable
// CHECK: $T11 conforms to Hashable
_ = List(arr, id: \.id)
