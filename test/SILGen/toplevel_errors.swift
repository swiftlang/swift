// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

enum MyError : ErrorType {
  case A, B
}

throw MyError.A

// CHECK: sil @main
// CHECK: [[ERR:%.*]] = alloc_existential_box $ErrorType, $MyError
// CHECK: [[ADDR:%.*]] = project_existential_box $MyError in [[ERR]] : $ErrorType
// CHECK: [[T0:%.*]] = enum $MyError, #MyError.A!enumelt
// CHECK: store [[T0]] to [[ADDR]] : $*MyError
// CHECK: builtin "willThrow"([[ERR]] : $ErrorType)
// CHECK: br bb2([[ERR]] : $ErrorType)

// CHECK: bb1([[T0:%.*]] : $Int32):
// CHECK: return [[T0]] : $Int32

// CHECK: bb2([[T0:%.*]] : $ErrorType):
// CHECK: builtin "errorInMain"([[T0]] : $ErrorType)
// CHECK: [[T0:%.*]] = integer_literal $Builtin.Int32, 1
// CHECK: [[T1:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK: br bb1([[T1]] : $Int32)
