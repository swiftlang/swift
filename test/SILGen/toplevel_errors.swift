// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

enum MyError : ErrorType {
  case A, B
}

throw MyError.A

// CHECK: sil @main
// CHECK: [[ERR:%.*]] = alloc_existential_box $ErrorType, $MyError
// CHECK: [[T0:%.*]] = function_ref @_TFO15toplevel_errors7MyError1AFMS0_S0_
// CHECK: [[T1:%.*]] = apply [[T0]](
// CHECK: store [[T1]] to [[ERR]]#1 : $*MyError
// CHECK: builtin "willThrow"([[ERR]]#0 : $ErrorType)
// CHECK: br bb3([[ERR]]#0 : $ErrorType)

// CHECK: bb1:
// CHECK: [[T0:%.*]] = integer_literal $Builtin.Int32, 0
// CHECK: [[T1:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK: br bb2([[T1]] : $Int32)

// CHECK: bb2([[T0:%.*]] : $Int32):
// CHECK: return [[T0]] : $Int32

// CHECK: bb3([[T0:%.*]] : $ErrorType):
// CHECK: builtin "errorInMain"([[T0]] : $ErrorType)
// CHECK: [[T0:%.*]] = integer_literal $Builtin.Int32, 1
// CHECK: [[T1:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK: br bb2([[T1]] : $Int32)