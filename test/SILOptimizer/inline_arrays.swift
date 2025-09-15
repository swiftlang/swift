// RUN: %target-swift-frontend  -primary-file %s -O -disable-availability-checking -module-name=test -emit-sil | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-OPT

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// CHECK-LABEL: sil @$s4test0A9Subscriptys5UInt8Vs11InlineArrayVy$255_ADGz_SitF :
// CHECK-OPT:      [[S:%.*]] = struct_element_addr %0, #InlineArray._storage
// CHECK-ONONE:    [[ACC:%.*]] = begin_access [read] [static] %0
// CHECK-ONONE:    [[S:%.*]] = struct_element_addr [[ACC]], #InlineArray._storage
// CHECK:          [[BA:%.*]] = vector_base_addr [[S]]
// CHECK:          [[EA:%.*]] = index_addr [stack_protection] [[BA]],
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
  // CHECK:          [[EA:%.*]] = index_addr [stack_protection] [[BA]],
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
  // CHECK:          [[EA:%.*]] = index_addr [stack_protection] [[BA]],
  // CHECK-OPT:      [[E:%.*]] = load [[EA]]
  // CHECK-ONONE:    [[ACC2:%.*]] = begin_access [read] [unsafe] [[EA]]
  // CHECK-ONONE:    [[E:%.*]] = load [[ACC2]]
  // CHECK:          return [[E]]
  // CHECK:       } // end sil function '$s4test1SV0A9Subscriptys5UInt8VSiF'
  public func testSubscript(_ i: Int) -> UInt8 {
    return a[i]
  }
}

