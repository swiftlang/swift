
// RUN: %target-swift-frontend -module-name source_location -Xllvm -sil-full-demangle -Xllvm -sil-print-debuginfo -emit-silgen -enable-sil-ownership %s | %FileCheck %s

func printSourceLocation(file: String = #file, line: Int = #line) {}

#sourceLocation(file: "caller.swift", line: 10000)
_ = printSourceLocation()
// CHECK: [[CALLER_FILE_VAL:%.*]] = string_literal utf16 "caller.swift",
// CHECK: [[CALLER_FILE:%.*]] = apply {{.*}}([[CALLER_FILE_VAL]],
// CHECK: [[BORROWED_CALLER_FILE:%.*]] = begin_borrow [[CALLER_FILE]]
// CHECK: [[CALLER_LINE_VAL:%.*]] = integer_literal $Builtin.Int{{[0-9]+}}, 10000,
// CHECK: [[CALLER_LINE:%.*]] = apply {{.*}}([[CALLER_LINE_VAL]],
// CHECK: [[PRINT_SOURCE_LOCATION:%.*]] = function_ref @$S15source_location19printSourceLocation4file4lineySS_SitF
// CHECK: apply [[PRINT_SOURCE_LOCATION]]([[BORROWED_CALLER_FILE]], [[CALLER_LINE]])

#sourceLocation(file: "inplace.swift", line: 20000)
let FILE = #file, LINE = #line
// CHECK: [[FILE_ADDR:%.*]] = global_addr @$S15source_location4FILESSv
// CHECK: [[INPLACE_FILE_VAL:%.*]] = string_literal utf16 "inplace.swift",
// CHECK: [[INPLACE_FILE:%.*]] = apply {{.*}}([[INPLACE_FILE_VAL]],
// CHECK: store [[INPLACE_FILE]] to [init] [[FILE_ADDR]]
// CHECK: [[LINE_ADDR:%.*]] = global_addr @$S15source_location4LINESiv
// CHECK: [[INPLACE_LINE_VAL:%.*]] = integer_literal $Builtin.Int{{[0-9]+}}, 20000,
// CHECK: [[INPLACE_LINE:%.*]] = apply {{.*}}([[INPLACE_LINE_VAL]],
// CHECK: store [[INPLACE_LINE]] to [trivial] [[LINE_ADDR]]
