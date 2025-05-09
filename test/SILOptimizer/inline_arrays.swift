// RUN: %target-swift-frontend  -primary-file %s -O -disable-availability-checking -module-name=test -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// CHECK-LABEL: sil @$s4test0A9Subscriptys5UInt8Vs11InlineArrayVy$255_ADGz_SitF :
// CHECK:          [[S:%.*]] = struct_element_addr %0, #InlineArray._storage
// CHECK:          [[BA:%.*]] = vector_base_addr [[S]]
// CHECK:          [[EA:%.*]] = index_addr [stack_protection] [[BA]],
// CHECK:          [[E:%.*]] = load [[EA]]
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

  // CHECK-LABEL: sil @$s4test1CC0A9Subscriptys5UInt8VSiF :
  // CHECK:          [[CA:%.*]] = ref_element_addr [immutable] %1, #C.a
  // CHECK:          [[S:%.*]] = struct_element_addr [[CA]], #InlineArray._storage
  // CHECK:          [[BA:%.*]] = vector_base_addr [[S]]
  // CHECK:          [[EA:%.*]] = index_addr [stack_protection] [[BA]],
  // CHECK:          [[E:%.*]] = load [[EA]]
  // CHECK:          return [[E]]
  // CHECK:       } // end sil function '$s4test1CC0A9Subscriptys5UInt8VSiF'
  public func testSubscript(_ i: Int) -> UInt8 {
    return a[i]
  }
}

