// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/lowered_function_types -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/lowered_function_types -type-from-mangled=%t/input | %FileCheck %s

func blackHole(_: Any...) {}

class Class {}

do {
  let fn: (Int, Class, __owned Class, Any, inout Int) -> (Int, Class, Any) = {
    _, _, _, _, _ in fatalError()
  }
  blackHole(fn)
}

do {
  let fn: () throws -> () = {}
  blackHole(fn)
}

// DEMANGLE: $sSi22lowered_function_types5ClassCACypS2iACypIegygxnldor_D
// DEMANGLE: $ss5Error_pIegzo_D
// DEMANGLE: $sSiSiIegco_D

// CHECK: @callee_guaranteed (Int, @guaranteed Class, @owned Class, @in_guaranteed Any, @inout Int) -> (Int, @owned Class, @out Any)
// CHECK: @callee_guaranteed () -> @error Error
// CHECK: @callee_guaranteed (@in_constant Int) -> @owned Int