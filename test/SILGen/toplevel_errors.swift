// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

enum MyError : ErrorProtocol {
  case A, B
}

throw MyError.A

// CHECK: sil @main
// CHECK: [[ERR:%.*]] = alloc_existential_box $ErrorProtocol, $MyError
// CHECK: [[T0:%.*]] = enum $MyError, #MyError.A!enumelt
// CHECK: store [[T0]] to [[ERR]]#1 : $*MyError
// CHECK: builtin "willThrow"([[ERR]]#0 : $ErrorProtocol)
// CHECK: br bb2([[ERR]]#0 : $ErrorProtocol)

// CHECK: bb1([[T0:%.*]] : $Int32):
// CHECK: return [[T0]] : $Int32

// CHECK: bb2([[T0:%.*]] : $ErrorProtocol):
// CHECK: builtin "errorInMain"([[T0]] : $ErrorProtocol)
// CHECK: [[T0:%.*]] = integer_literal $Builtin.Int32, 1
// CHECK: [[T1:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK: br bb1([[T1]] : $Int32)
