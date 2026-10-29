// RUN: %target-swift-emit-silgen -enable-builtin-module %s | %FileCheck %s

import Builtin

// CHECK-LABEL: sil hidden [ossa] @$s19builtin_assume_true5equalyySi_SitF :
// CHECK:         [[P:%.*]] = integer_literal $Builtin.Int8, 0
// CHECK-NEXT:    builtin "assumeTrue"<Int>(%0, %1, [[P]]) : $()
// CHECK:       } // end sil function '$s19builtin_assume_true5equalyySi_SitF'
func equal(_ x: Int, _ y: Int) {
  Builtin.assumeTrue(x, ==, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s19builtin_assume_true8notEqualyySi_SitF :
// CHECK:         [[P:%.*]] = integer_literal $Builtin.Int8, 1
// CHECK-NEXT:    builtin "assumeTrue"<Int>(%0, %1, [[P]]) : $()
// CHECK:       } // end sil function '$s19builtin_assume_true8notEqualyySi_SitF'
func notEqual(_ x: Int, _ y: Int) {
  Builtin.assumeTrue(x, !=, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s19builtin_assume_true8lessThanyySi_SitF :
// CHECK:         [[P:%.*]] = integer_literal $Builtin.Int8, 2
// CHECK-NEXT:    builtin "assumeTrue"<Int>(%0, %1, [[P]]) : $()
// CHECK:       } // end sil function '$s19builtin_assume_true8lessThanyySi_SitF'
func lessThan(_ x: Int, _ y: Int) {
  Builtin.assumeTrue(x, <, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s19builtin_assume_true15lessThanOrEqualyySi_SitF :
// CHECK:         [[P:%.*]] = integer_literal $Builtin.Int8, 3
// CHECK-NEXT:    builtin "assumeTrue"<Int>(%0, %1, [[P]]) : $()
// CHECK:       } // end sil function '$s19builtin_assume_true15lessThanOrEqualyySi_SitF'
func lessThanOrEqual(_ x: Int, _ y: Int) {
  Builtin.assumeTrue(x, <=, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s19builtin_assume_true11greaterThanyySi_SitF :
// CHECK:         [[P:%.*]] = integer_literal $Builtin.Int8, 2
// CHECK-NEXT:    builtin "assumeTrue"<Int>(%1, %0, [[P]]) : $()
// CHECK:       } // end sil function '$s19builtin_assume_true11greaterThanyySi_SitF'
func greaterThan(_ x: Int, _ y: Int) {
  Builtin.assumeTrue(x, >, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s19builtin_assume_true18greaterThanOrEqualyySi_SitF :
// CHECK:         [[P:%.*]] = integer_literal $Builtin.Int8, 3
// CHECK-NEXT:    builtin "assumeTrue"<Int>(%1, %0, [[P]]) : $()
// CHECK:       } // end sil function '$s19builtin_assume_true18greaterThanOrEqualyySi_SitF'
func greaterThanOrEqual(_ x: Int, _ y: Int) {
  Builtin.assumeTrue(x, >=, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s19builtin_assume_true8unsignedyys5UInt8V_ADtF :
// CHECK:         [[P:%.*]] = integer_literal $Builtin.Int8, 3
// CHECK-NEXT:    builtin "assumeTrue"<UInt8>(%0, %1, [[P]]) : $()
// CHECK:       } // end sil function '$s19builtin_assume_true8unsignedyys5UInt8V_ADtF'
func unsigned(_ x: UInt8, _ y: UInt8) {
  Builtin.assumeTrue(x, <=, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s19builtin_assume_true7genericyyx_xtSLRzlF :
// CHECK:       bb0(%0 : $*T, %1 : $*T):
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     copy_addr
// CHECK:         [[P:%.*]] = integer_literal $Builtin.Int8, 3
// CHECK-NEXT:    builtin "assumeTrue"<T>(%0, %1, [[P]]) : $()
// CHECK:       } // end sil function '$s19builtin_assume_true7genericyyx_xtSLRzlF'
func generic<T: Comparable>(_ x: T, _ y: T) {
  Builtin.assumeTrue(x, <=, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s19builtin_assume_true18genericGreaterThanyyx_xtSLRzlF :
// CHECK:         [[P:%.*]] = integer_literal $Builtin.Int8, 2
// CHECK-NEXT:    builtin "assumeTrue"<T>(%1, %0, [[P]]) : $()
// CHECK:       } // end sil function '$s19builtin_assume_true18genericGreaterThanyyx_xtSLRzlF'
func genericGreaterThan<T: Comparable>(_ x: T, _ y: T) {
  Builtin.assumeTrue(x, >, y)
}

// CHECK-LABEL: sil hidden [ossa] @$s19builtin_assume_true12genericEqualyyx_xtSQRzlF :
// CHECK:         [[P:%.*]] = integer_literal $Builtin.Int8, 0
// CHECK-NEXT:    builtin "assumeTrue"<T>(%0, %1, [[P]]) : $()
// CHECK:       } // end sil function '$s19builtin_assume_true12genericEqualyyx_xtSQRzlF'
func genericEqual<T: Equatable>(_ x: T, _ y: T) {
  Builtin.assumeTrue(x, ==, y)
}
