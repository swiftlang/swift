// RUN: %target-swift-frontend -primary-file %s -O -sil-verify-all -module-name=test -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib


// Check that we can eliminate all optionals from a loop which is iterating
// over an array of address-only elements.

// CHECK-LABEL: sil @$s4test0A18_no_optionals_usedyySayxGlF : $@convention(thin) <T> (@guaranteed Array<T>) -> () {
// CHECK-NOT: Optional
// CHECK: } // end sil function '$s4test0A18_no_optionals_usedyySayxGlF'
public func test_no_optionals_used<T>(_ items: [T]) {
  for i in items {
    print(i)
  }
}

