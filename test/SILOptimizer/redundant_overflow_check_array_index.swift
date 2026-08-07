// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// `i + c` (c > 0) as an array index can't observably overflow: the bounds check traps first.

// CHECK-LABEL: sil {{.*}}9nextValue
// CHECK-NOT: "arithmetic overflow"
// CHECK: "Index out of range"
// CHECK: return
public func nextValue(_ a: [Int], _ i: Int) -> Int {
  return a[i + 1]
}

// A negative constant can underflow into a valid index, so the check stays.
// CHECK-LABEL: sil {{.*}}13previousValue
// CHECK: "arithmetic overflow"
public func previousValue(_ a: [Int], _ i: Int) -> Int {
  return a[i - 1]
}
