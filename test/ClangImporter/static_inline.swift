// RUN: %empty-directory(%t)

// Check if SIL printing+parsing of a clang imported function works.

// RUN: %target-swift-frontend -parse-as-library -module-name=static_inline -emit-sil %S/Inputs/static_inline.swift -enable-objc-interop -import-objc-header %S/Inputs/static_inline.h -o %t/static_inline.sil
// RUN: %FileCheck < %t/static_inline.sil %s
// RUN: %target-swift-frontend -parse-as-library -module-name=static_inline -O -emit-ir %t/static_inline.sil -enable-objc-interop -import-objc-header %S/Inputs/static_inline.h | %FileCheck --check-prefix=CHECK-IR %s

// CHECK: sil shared [asmname "c_inline_func"] [clang c_inline_func] @$sSo13c_inline_funcys5Int32VACFTo : $@convention(c) (Int32) -> Int32

// CHECK-IR-LABEL: define{{.*}} i32 @"$s13static_inline6testit1xs5Int32VAE_tF"(i32 %0)
// CHECK-IR: = add {{.*}}, 27
// CHECK-IR: ret

import static_inline

public func mytest(x: Int32) -> Int32 {
  return testit(x: x)
}
