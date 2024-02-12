// RUN: %target-swift-frontend  -primary-file %s -O -module-name=test -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend  -primary-file %s -O -module-name=test -emit-ir | %FileCheck %s -check-prefix=CHECK-LLVM

// Also do an end-to-end test to check all components, including IRGen.
// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: swift_in_compiler,executable_test,swift_stdlib_no_asserts,optimized_stdlib

// Check that we create reasonable optimized code for this function.

@inline(never)
public func reverseArray(_ a: [Int]) -> [Int] {
  var new: [Int] = []
  new.reserveCapacity(a.count)

  var fromIndex = a.count &- 1

  while fromIndex >= 0 {
    new.append(a[fromIndex])
    fromIndex &-= 1
  }

  return new
}


// CHECK-LABEL: sil [noinline] {{.*}}@$s4test12reverseArrayySaySiGACF

// There must not be more than two begin_cow_mutation - end_cow_mutation pairs:
// * the first one for the initial reserveCapacity
// * the second for the append.

// CHECK-NOT: {{.*(_cow_mutation|cond_fail)}}
// CHECK:     begin_cow_mutation
// CHECK-NOT: {{.*(_cow_mutation|cond_fail)}}
// CHECK:     end_cow_mutation
// CHECK:     end_cow_mutation

// In SIL we fail to eliminate the bounds check of the input array.
// But that's okay, because LLVM can do that.
// So we accept one cond_fail in the SIL output.

// CHECK:     cond_fail {{.*}} "Index out of range"

// The second begin_cow_mutation - end_cow_mutation pair:

// CHECK-NOT: {{.*(_cow_mutation|cond_fail)}}
// CHECK:     begin_cow_mutation
// CHECK-NOT: {{.*(_cow_mutation|cond_fail)}}
// CHECK:     end_cow_mutation
// CHECK-NOT: {{.*(_cow_mutation|cond_fail)}}

// CHECK: } // end sil function '$s4test12reverseArrayySaySiGACF'


// Check that there are no cond_fails left in the LLVM output.
// LLVM should be able to optimize away the bounds check of the input array.

// CHECK-LLVM-LABEL: define {{.*}} @"$s4test12reverseArrayySaySiGACF"
// CHECK-LLVM-NOT:     llvm.trap
// CHECK-LLVM:       }


// CHECK-OUTPUT: [3, 2, 1]
print(reverseArray([1, 2, 3]))

