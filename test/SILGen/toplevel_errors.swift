// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

enum MyError : Error {
  case A, B
}

throw MyError.A

// CHECK: sil @main
// CHECK: [[ERR:%.*]] = alloc_existential_box $Error, $MyError
// CHECK: [[ADDR:%.*]] = project_existential_box $MyError in [[ERR]] : $Error
// CHECK: [[ENUM_CASE:%.*]] = function_ref @$S15toplevel_errors7MyErrorO1AyA2CmF : $@convention(method) (@thin MyError.Type) -> MyError
// CHECK: [[T0:%.*]] = apply [[ENUM_CASE]]({{%.*}})
// CHECK: store [[T0]] to [trivial] [[ADDR]] : $*MyError
// CHECK: builtin "willThrow"([[ERR]] : $Error)
// CHECK: br bb2([[ERR]] : $Error)

// CHECK: bb1([[T0:%.*]] : $Int32):
// CHECK: return [[T0]] : $Int32

// CHECK: bb2([[T0:%.*]] : $Error):
// CHECK: builtin "errorInMain"([[T0]] : $Error)
// CHECK: [[T0:%.*]] = integer_literal $Builtin.Int32, 1
// CHECK: [[T1:%.*]] = struct $Int32 ([[T0]] : $Builtin.Int32)
// CHECK: br bb1([[T1]] : $Int32)
