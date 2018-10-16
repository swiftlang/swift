// RUN: %empty-directory(%t)

// Try serialization of clang-generated functions.

// RUN: %target-swift-frontend -parse-as-library -module-name=static_inline -emit-module %S/Inputs/static_inline.swift -enable-objc-interop -import-objc-header %S/Inputs/static_inline.h -o %t/static_inline.swiftmodule
// RUN: %target-swift-frontend -module-name test -O -emit-sil %s -I %t -enable-objc-interop | %FileCheck %s
// RUN: %target-swift-frontend -module-name test -O -emit-ir %s -I %t -enable-objc-interop | %FileCheck --check-prefix=CHECK-IR %s

// CHECK: sil shared_external [clang c_inline_func] @c_inline_func : $@convention(c) (Int32) -> Int32

// CHECK-IR-LABEL: define{{.*}} i32 @"$s4test6mytest1xs5Int32VAE_tF"(i32)
// CHECK-IR: = add {{.*}}, 27
// CHECK-IR: ret

import static_inline

public func mytest(x: Int32) -> Int32 {
  return testit(x: x)
}
