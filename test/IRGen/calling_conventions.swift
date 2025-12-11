// RUN: %target-swift-frontend -module-name calling_conventions -use-clang-function-types -parse-as-library -I %S/Inputs -emit-ir %s | %FileCheck %s

import CallingConventions

func foo() {
  // CHECK: call i32 @cdecl_fn(i32 1, i32 2, i32 3)
  _ = cdecl_fn(1, 2, 3)

  // CHECK: call i32 %{{[0-9]+}}(i32 4, i32 5, i32 6)
  _ = cdecl_fn_ptr(4, 5, 6)

  // CHECK: call swiftcc i32 @swiftcc_fn(i32 1, i32 2, i32 3)
  _ = swiftcc_fn(1, 2, 3)

  // CHECK: call swiftcc i32 %{{[0-9]+}}(i32 4, i32 5, i32 6)
  _ = swiftcc_fn_ptr(4, 5, 6)
}
