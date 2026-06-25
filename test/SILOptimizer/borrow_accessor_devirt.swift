// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

class RefCounted {
  var data: [Int]
  init(_ d: [Int]) { self.data = d }
}

protocol HasRef {
  var ref: RefCounted { borrow mutate }
}

struct ConformingWrapper: HasRef {
  var _ref: RefCounted
  var ref: RefCounted {
    borrow { return _ref }
    mutate { return &_ref }
  }
  init(_ d: [Int]) { _ref = RefCounted(d) }
}


// CHECK-LABEL: sil hidden [noinline] @$s{{.*}}10testDevirt
// CHECK-NOT: witness_method
// CHECK-NOT: open_existential_addr
// CHECK: ref_tail_addr
// CHECK: } // end sil function '$s{{.*}}10testDevirt

@inline(never)
func testDevirt() -> Int {
  let w: any HasRef = ConformingWrapper([5, 10, 15])
  let borrowed = w.ref
  return borrowed.data.reduce(0, +)
}

print(testDevirt())

