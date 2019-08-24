// RUN: %target-swift-frontend -swift-version 4 -O %s -emit-sil | %FileCheck %s

// This test is testing that even in presence of
// RangeReplaceableCollection.filter(_:), Arrays are still calling the default
// implementation from Sequence.

// CHECK-NOT: RangeReplaceableCollection.filter(_:)
@inline(never)
public func foobar(_ xs: [Int]) -> [Int] {
  return xs.filter { _ in false }
}

