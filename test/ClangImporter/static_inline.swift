// RUN: rm -rf %t && mkdir -p %t

// Check if SIL printing+parsing of a clang imported function works.

// RUN: %target-swift-frontend -parse-as-library -module-name=test -emit-sil %s -import-objc-header %S/Inputs/static_inline.h -o %t/test.sil
// RUN: %FileCheck < %t/test.sil %s
// RUN: %target-swift-frontend -parse-as-library -module-name=test -O -emit-ir %t/test.sil -import-objc-header %S/Inputs/static_inline.h | %FileCheck --check-prefix=CHECK-IR %s

// CHECK: sil shared [clang c_inline_func] @c_inline_func : $@convention(c) (Int32) -> Int32

// CHECK-IR-LABEL: define{{.*}} i32 @_T04test6testits5Int32VAD1x_tF(i32)
// CHECK-IR: = add {{.*}}, 27
// CHECK-IR: ret

public func testit(x: Int32) -> Int32 {
  return c_inline_func(x)
}
