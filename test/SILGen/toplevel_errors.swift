// RUN: %target-swift-emit-silgen %s | %FileCheck %s

enum MyError : Error {
  case A, B
}

throw MyError.A

// CHECK: sil @main
// CHECK: [[T0:%.*]] = enum $MyError, #MyError.A!enumelt
// CHECK: [[ERR:%.*]] = alloc_existential_box $Error, $MyError
// CHECK: [[ADDR:%.*]] = project_existential_box $MyError in [[ERR]] : $Error
// CHECK: store [[ERR]] to [init] [[ERRBUF:%.*]] :
// CHECK: store [[T0]] to [trivial] [[ADDR]] : $*MyError
// CHECK: [[ERR2:%.*]] = load [take] [[ERRBUF]]
// CHECK: builtin "willThrow"([[ERR2]] : $Error)
// CHECK: br bb2([[ERR2]] : $Error)

// CHECK: bb1([[T0:%.*]] : @trivial $Int32):
// CHECK: return [[T0]] : $Int32

// CHECK: bb2([[T0:%.*]] : @owned $Error):
// CHECK: builtin "errorInMain"([[T0]] : $Error)
// CHECK: [[T0:%.*]] = integer_literal $Builtin.Int32, 1
// CHECK: [[T1:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK: br bb1([[T1]] : $Int32)
