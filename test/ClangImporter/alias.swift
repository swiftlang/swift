// RUN: %empty-directory(%t)
// RUN: %target-typcheck-verify-swift -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend -I %S/Inputs/custom-modules -parse-as-library -module-name Alias -Osize -emit-ir -o - %s | %FileCheck %s -check-prefix CHECK-ANSI-IR
// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules %s -Xcc -DUNICODE
// RUN: %target-swift-frontend -I %S/Inputs/custom-modules -parse-as-library -module-name Alias -Osize -emit-ir -o - %s -Xcc -DUNICODE | %FileCheck %s -check-prefix CHECK-UNICODE-IR

// expected-no-diagnostics

import Aliases

func f() {
  F(V)
}

func g() {
  UI = 32
}

// CHECK-ANSI-IR: @VA = external dso_local local_unnamed_addr constant i32
// CHECK-ANSI-IR: @UIA = external dso_local local_unnamed_addr global float

// CHECK-ANSI-IR: define hidden swiftcc void @"$s5Alias1fyyF"(){{.*}}{
// CHECK-ANSI-IR: entry:
// CHECK-ANSI-IR:   %0 = load i32, ptr @VA, align 8
// CHECK-ANSI-IR:   tail call void @FA(i32 %0)
// CHECK-ANSI-IR:   ret void
// CHECK-ANSI-IR: }

// CHECK-ANSI-IR: declare dso_local void @FA(i32 noundef)
// CHECK-ANSI-IR-NOT: declare dso_local void @FW(i64 noundef)

// CHECK-ANSI-IR: define hidden swiftcc void @"$s5Alias1gyyF"(){{.*}}{
// CHECK-ANSI-IR: entry:
// CHECK-ANSI-IR:   store float 3.200000e+01, ptr @UIA
// CHECK-ANSI-IR:   ret void
// CHECK-ANSI-IR: }

// CHECK-UNICODE-IR: @VW = external dso_local local_unnamed_addr constant i64
// CHECK-UNICODE-IR: @UIW = external dso_local local_unnamed_addr global double

// CHECK-UNICODE-IR: define hidden swiftcc void @"$s5Alias1fyyF"(){{.*}}{
// CHECK-UNICODE-IR: entry:
// CHECK-UNICODE-IR:   %0 = load i64, ptr @VW, align 8
// CHECK-UNICODE-IR:   tail call void @FW(i64 %0)
// CHECK-UNICODE-IR:   ret void
// CHECK-UNICODE-IR: }

// CHECK-UNICODE-IR: declare dso_local void @FW(i64 noundef)
// CHECK-UNICODE-IR-NOT: declare dso_local void @FA(i32 noundef)

// CHECK-UNICODE-IR: define hidden swiftcc void @"$s5Alias1gyyF"(){{.*}}{
// CHECK-UNICODE-IR: entry:
// CHECK-UNICODE-IR:   store double 3.200000e+01, ptr @UIW
// CHECK-UNICODE-IR:   ret void
// CHECK-UNICODE-IR: }
