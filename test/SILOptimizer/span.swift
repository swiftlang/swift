// RUN: %target-swift-frontend %s -parse-as-library -disable-availability-checking -emit-ir -O | %FileCheck %s --check-prefix=CHECK

// Check that the MoveOnlyWrappedTypeEliminator doesn't crash
func consumingArray(_ arr: consuming [Int]) {
  let s = arr.mutableSpan;
  _ = consume s
}


// Check that the span element is borrowed, not copied.
// rdar://183793878
// CHECK:      define {{.*}} @"$s{{.*}}9s_dynamic{{.*}} {
// CHECK-NOT:    alloca
// CHECK:        getelementptr
// CHECK-NEXT:   getelementptr
// CHECK-NEXT:   load
// CHECK-NEXT:   ret
// CHECK:      }
public func s_dynamic(_ a: inout [10 of [10 of UInt64]], i: Int) -> UInt64 {
  a.span[i][i]
}

