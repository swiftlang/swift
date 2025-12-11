// RUN: %swift-frontend-plain -target i686-unknown-windows-msvc -module-name win32_calling_conventions -use-clang-function-types -parse-as-library -I %S/Inputs -emit-ir %s | %FileCheck %s

// REQUIRES: OS=windows-msvc

import Win32CallingConventions

func foo() {
  // CHECK: call x86_stdcallcc i32 @cdecl_fn(i32 1, i32 2, i32 3)
  stdcall_fn(1, 2, 3)

  // CHECK: call x86_stdcallcc i32 %{{[0-9]+}}(i32 4, i32 5, i32 6)
  stdcall_fn_ptr(4, 5, 6)

  // CHECK: call x86_fastcallcc i32 @cdecl_fn(i32 1, i32 2, i32 3)
  fastcall_fn(1, 2, 3)

  // CHECK: call x86_fastcallcc i32 %{{[0-9]+}}(i32 4, i32 5, i32 6)
  fastcall_fn_ptr(4, 5, 6)

  // CHECK: call x86_vectorcallcc i32 @cdecl_fn(i32 1, i32 2, i32 3)
  vectorcall_fn(1, 2, 3)

  // CHECK: call x86_vectorcallcc i32 %{{[0-9]+}}(i32 4, i32 5, i32 6)
  vectorcall_fn_ptr(4, 5, 6)
}
