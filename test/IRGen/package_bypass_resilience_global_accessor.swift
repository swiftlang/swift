// RUN: %empty-directory(%t)

// RUN: %target-build-swift -module-name=File -package-name Pkg -I%t -emit-sil %s | %FileCheck %s --check-prefix=CHECK-SIL
// RUN: %target-build-swift -module-name=File -package-name Pkg -I%t -emit-sil %s -enable-library-evolution | %FileCheck %s --check-prefix=CHECK-SIL-HID
// RUN: %target-build-swift -module-name=File -package-name Pkg -I%t -emit-sil %s -enable-library-evolution -Xfrontend -experimental-allow-non-resilient-access | %FileCheck %s --check-prefix=CHECK-SIL
// RUN: %target-build-swift -module-name=File -package-name Pkg -I%t -emit-ir %s | %FileCheck %s --check-prefix=CHECK-IR
// RUN: %target-build-swift -module-name=File -package-name Pkg -I%t -emit-ir %s -enable-library-evolution | %FileCheck %s --check-prefix=CHECK-IR-HID
// RUN: %target-build-swift -module-name=File -package-name Pkg -I%t -emit-ir %s -enable-library-evolution -Xfrontend -experimental-allow-non-resilient-access | %FileCheck %s --check-prefix=CHECK-IR

public struct S {
  public static var x = "hello world"
}

// S.x.unsafeMutableAddressor
// CHECK-SIL: sil [global_init] @$s4File1SV1xSSvau : $@convention(thin) () -> Builtin.RawPointer {
// S.x.unsafeMutableAddressor
// CHECK-SIL-HID: sil hidden [global_init] @$s4File1SV1xSSvau : $@convention(thin) () -> Builtin.RawPointer {

// CHECK-IR: define{{( dllexport)?}}{{( protected)?}} swiftcc ptr @"$s4File1SV1xSSvau"()
// CHECK-IR-HID: define hidden swiftcc ptr @"$s4File1SV1xSSvau"()

