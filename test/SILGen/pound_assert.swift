// RUN: %target-swift-frontend -enable-experimental-static-assert -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden [ossa] @$s12pound_assert15noCustomMessage{{[_0-9a-zA-Z]*}}
func noCustomMessage() {
  #assert(true)
  // CHECK: [[LOGIC_VALUE:%.*]] = struct_extract {{.*}}
  // CHECK-NEXT: [[MESSAGE:%.*]] = string_literal utf8 ""
  // CHECK-NEXT: builtin "poundAssert"([[LOGIC_VALUE]] : $Builtin.Int1, [[MESSAGE]] : $Builtin.RawPointer)
}
// CHECK: } // end sil function '$s12pound_assert15noCustomMessage{{[_0-9a-zA-Z]*}}'

// CHECK-LABEL: sil hidden [ossa] @$s12pound_assert13customMessage{{[_0-9a-zA-Z]*}}
func customMessage() {
  #assert(true, "custom message")
  // CHECK: [[LOGIC_VALUE:%.*]] = struct_extract {{.*}}
  // CHECK-NEXT: [[MESSAGE:%.*]] = string_literal utf8 "custom message"
  // CHECK-NEXT: builtin "poundAssert"([[LOGIC_VALUE]] : $Builtin.Int1, [[MESSAGE]] : $Builtin.RawPointer)
}
// CHECK: } // end sil function '$s12pound_assert13customMessage{{[_0-9a-zA-Z]*}}'
