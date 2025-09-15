// RUN: %target-swift-frontend  -primary-file %s -module-name=test -Xllvm -sil-print-types -Xllvm -sil-print-debuginfo -emit-sil | %FileCheck %s

// REQUIRES: swift_in_compiler

// is_same_metatype builtin is no longer used due to rdar://145707064 (Builtin.is_same_metatype should support noncopyable/nonescapable types)
// XFAIL: rdar145707064

// CHECK-LABEL: sil [transparent] @$s4test9checkTypeySixs17FixedWidthIntegerRzlF
@_transparent
public func checkType<A: FixedWidthInteger>(_ a: A) -> Int {
  // CHECK-NOT:     builtin
  // CHECK:         debug_step , loc "{{[^"]+}}":[[# @LINE + 1]]
  if _isConcrete(A.self) {
    // CHECK-NOT:     builtin
    if A.self == Int.self {
      return 1
    }
  }
  return 0
}
// CHECK:       } // end sil function '$s4test9checkTypeySixs17FixedWidthIntegerRzlF'

// CHECK-LABEL: sil @$s4test0A10IsConcreteSiyF
public func testIsConcrete() -> Int {
  // CHECK:         debug_step , loc "{{[^"]+}}":[[# @LINE + 1]]:3
  checkType(1)
  // CHECK:         [[IL:%[0-9]+]] = integer_literal $Builtin.Int{{[0-9]+}}, 1
  // CHECK:         [[I:%[0-9]+]] = struct $Int ([[IL]] :
  // CHECK:         return [[I]]
}
// CHECK:       } // end sil function '$s4test0A10IsConcreteSiyF'


// CHECK-LABEL: sil @$s4test0A17MetadatComparisonSbyF
public func testMetadatComparison() -> Bool {
  // CHECK:         debug_step , loc "{{[^"]+}}":[[# @LINE + 1]]
  [String: Int].self == Never.self
  // CHECK:         [[IL:%[0-9]+]] = integer_literal $Builtin.Int1, 0
  // CHECK:         [[I:%[0-9]+]] = struct $Bool ([[IL]] :
  // CHECK:         return [[I]]
}
// CHECK:       } // end sil function '$s4test0A17MetadatComparisonSbyF'
