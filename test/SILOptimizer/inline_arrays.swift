// RUN: %target-swift-frontend  -primary-file %s -O -disable-availability-checking -module-name=test -emit-sil | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-OPT

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib
// REQUIRES: PTRSIZE=64

// CHECK-LABEL: sil @$s4test0A9Subscriptys5UInt8Vs11InlineArrayVy$255_ADGz_SitF :
// CHECK-OPT:      [[S:%.*]] = struct_element_addr %0, #InlineArray._storage
// CHECK-ONONE:    [[ACC:%.*]] = begin_access [read] [static] %0
// CHECK-ONONE:    [[S:%.*]] = struct_element_addr [[ACC]], #InlineArray._storage
// CHECK:          [[BA:%.*]] = vector_base_addr [[S]]
// CHECK:          [[EA:%.*]] = index_addr [stack_protection] [projection] [[BA]],
// CHECK-OPT:      [[E:%.*]] = load [[EA]]
// CHECK-ONONE:    [[ACC2:%.*]] = begin_access [read] [unsafe] [[EA]]
// CHECK-ONONE:    [[E:%.*]] = load [[ACC2]]
// CHECK:          return [[E]]
// CHECK:       } // end sil function '$s4test0A9Subscriptys5UInt8Vs11InlineArrayVy$255_ADGz_SitF'
public func testSubscript(_ a: inout InlineArray<256, UInt8>, _ i: Int) -> UInt8 {
  return a[i]
}

public final class C {
  let a: InlineArray<256, UInt8>

  init(_ a: InlineArray<256, UInt8>) {
    self.a = a
  }

  // CHECK-LABEL:    sil @$s4test1CC1iACs5UInt8V_tcfc
  // CHECK-OPT-NOT:    alloc_stack
  // CHECK:       } // end sil function '$s4test1CC1iACs5UInt8V_tcfc'
  public init(i: UInt8) {
    self.a = .init(repeating: i)
  }

  // CHECK-LABEL: sil @$s4test1CC0A9Subscriptys5UInt8VSiF :
  // CHECK:          [[CA:%.*]] = ref_element_addr [immutable] %1, #C.a
  // CHECK:          [[S:%.*]] = struct_element_addr [[CA]], #InlineArray._storage
  // CHECK:          [[BA:%.*]] = vector_base_addr [[S]]
  // CHECK:          [[EA:%.*]] = index_addr [stack_protection] [projection] [[BA]],
  // CHECK-OPT:      [[E:%.*]] = load [[EA]]
  // CHECK-ONONE:    [[ACC2:%.*]] = begin_access [read] [unsafe] [[EA]]
  // CHECK-ONONE:    [[E:%.*]] = load [[ACC2]]
  // CHECK:          return [[E]]
  // CHECK:       } // end sil function '$s4test1CC0A9Subscriptys5UInt8VSiF'
  public func testSubscript(_ i: Int) -> UInt8 {
    return a[i]
  }
}

public struct S {
  let a: InlineArray<7000, UInt8>

  // CHECK-LABEL: sil @$s4test1SV0A9Subscriptys5UInt8VSiF :
  // CHECK:          [[A:%.*]] = struct_element_addr %1, #S.a
  // CHECK:          [[S:%.*]] = struct_element_addr [[A]], #InlineArray._storage
  // CHECK:          [[BA:%.*]] = vector_base_addr [[S]]
  // CHECK:          [[EA:%.*]] = index_addr [stack_protection] [projection] [[BA]],
  // CHECK-OPT:      [[E:%.*]] = load [[EA]]
  // CHECK-ONONE:    [[ACC2:%.*]] = begin_access [read] [unsafe] [[EA]]
  // CHECK-ONONE:    [[E:%.*]] = load [[ACC2]]
  // CHECK:          return [[E]]
  // CHECK:       } // end sil function '$s4test1SV0A9Subscriptys5UInt8VSiF'
  public func testSubscript(_ i: Int) -> UInt8 {
    return a[i]
  }
}

// rdar://172132077 (Iterating over InlineArray copies the full array per iteration)

// CHECK-LABEL: sil @$s4test0A3Sum1aSis11InlineArrayVy$511_SiG_tF : $@convention(thin) (InlineArray<512, Int>) -> Int {
// CHECK:       bb0(%0 : $InlineArray<512, Int>):
// CHECK:         [[ALLOC:%[0-9]+]] = alloc_stack $InlineArray<512, Int>
// CHECK:         store %0 to [[ALLOC]]
// CHECK:         [[VECTOR:%[0-9]+]] = struct_element_addr [[ALLOC]], #InlineArray._storage
// CHECK:         [[VECTOR_BASE:%[0-9]+]] = vector_base_addr [[VECTOR]]
// CHECK:       bb1([[SUM:%[0-9]+]] : $Builtin.Int64, [[INDEX:%[0-9]+]] : $Builtin.Int64):
// CHECK-NOT:     alloc_stack $InlineArray<512, Int>
// CHECK:         [[INDEX_ADDR:%[0-9]+]] = index_addr [stack_protection] [projection] [[VECTOR_BASE]]
// CHECK:         [[VALUE_ADDR:%[0-9]+]] = struct_element_addr [[INDEX_ADDR]], #Int._value
// CHECK:         load [[VALUE_ADDR]]
// CHECK:       bb2:
// CHECK-NEXT:    br bb1
// CHECK:       bb3:
// CHECK-NEXT:    dealloc_stack [[ALLOC]]
// CHECK-NEXT:    return
// CHECK:       } // end sil function '$s4test0A3Sum1aSis11InlineArrayVy$511_SiG_tF'
public func testSum(a: InlineArray<512, Int>) -> Int {
  var sum = 0
  for i in a.indices {
    sum += a[i]
  }
  return sum
}

// CHECK-LABEL: sil @$s4test0A9BorrowSum1aSis11InlineArrayVy$511_SiG_tF : $@convention(thin) (InlineArray<512, Int>) -> Int {
// CHECK:       bb0(%0 : @noImplicitCopy $InlineArray<512, Int>):
// CHECK:         [[ALLOC:%[0-9]+]] = alloc_stack $InlineArray<512, Int>
// CHECK:         store %0 to [[ALLOC]]
// CHECK:         [[VECTOR:%[0-9]+]] = struct_element_addr [[ALLOC]], #InlineArray._storage
// CHECK:         [[VECTOR_BASE:%[0-9]+]] = vector_base_addr [[VECTOR]]
// CHECK:       bb1([[SUM:%[0-9]+]] : $Builtin.Int64, [[INDEX:%[0-9]+]] : $Builtin.Int64):
// CHECK-NOT:     alloc_stack $InlineArray<512, Int>
// CHECK:         [[INDEX_ADDR:%[0-9]+]] = index_addr [stack_protection] [projection] [[VECTOR_BASE]]
// CHECK:         [[VALUE_ADDR:%[0-9]+]] = struct_element_addr [[INDEX_ADDR]], #Int._value
// CHECK:         load [[VALUE_ADDR]]
// CHECK:       bb2:
// CHECK-NEXT:    br bb1
// CHECK:       bb3:
// CHECK-NEXT:    dealloc_stack [[ALLOC]]
// CHECK-NEXT:    return
// CHECK:       } // end sil function '$s4test0A9BorrowSum1aSis11InlineArrayVy$511_SiG_tF'
public func testBorrowSum(a: borrowing InlineArray<512, Int>) -> Int {
  var sum = 0
  for i in a.indices {
    sum += a[i]
  }
  return sum
}

// CHECK-LABEL: sil @$s4test0A11BorrowEqualySbs11InlineArrayVy$31_SiG_AEtF : $@convention(thin) (InlineArray<32, Int>, InlineArray<32, Int>) -> Bool {
// CHECK:       bb0(%0 : @noImplicitCopy $InlineArray<32, Int>, %1 : @noImplicitCopy $InlineArray<32, Int>):
// CHECK-NEXT:    [[LHS_ALLOC:%[0-9]+]] = alloc_stack $InlineArray<32, Int>
// CHECK-NEXT:    store %0 to [[LHS_ALLOC]]
// CHECK-NEXT:    [[RHS_ALLOC:%[0-9]+]] = alloc_stack $InlineArray<32, Int>
// CHECK-NEXT:    store %1 to [[RHS_ALLOC]]
// CHECK:       bb1({{.*}}):
// CHECK-NOT:     = alloc_stack
// CHECK:       bb2:
// CHECK-NOT:     = alloc_stack
// CHECK-NOT:     store %0
// CHECK-NOT:     store %1
// CHECK:       bb6({{.*}}):
// CHECK:         dealloc_stack [[RHS_ALLOC]]
// CHECK:         dealloc_stack [[LHS_ALLOC]]
// CHECK:       } // end sil function '$s4test0A11BorrowEqualySbs11InlineArrayVy$31_SiG_AEtF'
public func testBorrowEqual(_ lhs: borrowing [32 of Int], _ rhs: borrowing [32 of Int]) -> Bool {
    for i in 0..<32 {
        guard lhs[i] == rhs[i] else {
            return false
        }
    }
    return true
}
