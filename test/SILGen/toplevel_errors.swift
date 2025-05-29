// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-async-top-level %s | %FileCheck %s

enum MyError : Error {
  case A, B
}

throw MyError.A

// CHECK: sil [ossa] @main
// CHECK: [[T0:%.*]] = enum $MyError, #MyError.A!enumelt
// CHECK: [[ERR:%.*]] = alloc_existential_box $any Error, $MyError
// CHECK: [[ADDR:%.*]] = project_existential_box $MyError in [[ERR]] : $any Error
// CHECK: store [[ERR]] to [init] [[ERRBUF:%.*]] :
// CHECK: store [[T0]] to [trivial] [[ADDR]] : $*MyError
// CHECK: [[ERR2:%.*]] = load [take] [[ERRBUF]]
// CHECK: builtin "willThrow"([[ERR2]] : $any Error)
// CHECK: br bb2([[ERR2]] : $any Error)

// CHECK: bb1([[T0:%.*]] : $Int32):
// CHECK: return [[T0]] : $Int32

// CHECK: bb2([[T0:%.*]] : @owned $any Error):
// CHECK: builtin "errorInMain"([[T0]] : $any Error)
// CHECK: end_lifetime [[T0]]
// CHECK: [[T0:%.*]] = integer_literal $Builtin.Int32, 1
// CHECK: [[T1:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK: br bb1([[T1]] : $Int32)
