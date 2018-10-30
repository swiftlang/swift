// RUN: %target-swift-frontend -enable-experimental-static-assert -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden @$s12pound_assert15noCustomMessage{{[_0-9a-zA-Z]*}}
func noCustomMessage() {
  #assert(true)
  // CHECK: [[GET_LOGIC_VALUE:%.*]] = function_ref {{.*}}_getBuiltinLogicValue
  // CHECK-NEXT: [[LOGIC_VALUE:%.*]] = apply [[GET_LOGIC_VALUE]]
  // CHECK-NEXT: [[MESSAGE:%.*]] = string_literal utf8 ""
  // CHECK-NEXT: builtin "poundAssert"([[LOGIC_VALUE]] : $Builtin.Int1, [[MESSAGE]] : $Builtin.RawPointer)
}

// CHECK-LABEL: sil hidden @$s12pound_assert13customMessage{{[_0-9a-zA-Z]*}}
func customMessage() {
  #assert(true, "custom message")
  // CHECK: [[GET_LOGIC_VALUE:%.*]] = function_ref {{.*}}_getBuiltinLogicValue
  // CHECK-NEXT: [[LOGIC_VALUE:%.*]] = apply [[GET_LOGIC_VALUE]]
  // CHECK-NEXT: [[MESSAGE:%.*]] = string_literal utf8 "custom message"
  // CHECK-NEXT: builtin "poundAssert"([[LOGIC_VALUE]] : $Builtin.Int1, [[MESSAGE]] : $Builtin.RawPointer)
}
