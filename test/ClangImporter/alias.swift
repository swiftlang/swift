// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules %s -enable-experimental-feature ImportMacroAliases
// RUN: %target-swift-frontend -I %S/Inputs/custom-modules -parse-as-library -module-name Alias -Osize -emit-ir -o - %s -enable-experimental-feature ImportMacroAliases | %FileCheck %s -check-prefix CHECK-ANSI-IR
// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules %s -Xcc -DUNICODE -enable-experimental-feature ImportMacroAliases
// RUN: %target-swift-frontend -I %S/Inputs/custom-modules -parse-as-library -module-name Alias -Osize -emit-ir -o - %s -Xcc -DUNICODE -enable-experimental-feature ImportMacroAliases | %FileCheck %s -check-prefix CHECK-UNICODE-IR
// RUN: not %target-swift-frontend -I %S/Inputs/custom-modules -parse-as-library -module-name Alias -c %s -DINVALID -o /dev/null 2>&1 -enable-experimental-feature ImportMacroAliases | %FileCheck --dry-run %s -check-prefix CHECK-INVALID
// RUN: %target-swift-frontend -I %S/Inputs/custom-modules -parse-as-library -module-name Alias -emit-sil %s -o - -enable-experimental-feature ImportMacroAliases | %FileCheck %s -check-prefix CHECK-FUNCTION-POINTER

// REQUIRES: swift_feature_ImportMacroAliases

// expected-no-diagnostics

import Aliases

public func f() {
  F(V)
}

public func g() {
  UI = 32
}

// CHECK-ANSI-IR: @VA = external {{(dso_local )?}}local_unnamed_addr constant i32
// CHECK-ANSI-IR: @UIA = external {{(dso_local )?}}local_unnamed_addr global float

// CHECK-ANSI-IR: define {{.*}}swiftcc void @"$s5Alias1fyyF"(){{.*}}{
// CHECK-ANSI-IR: entry:
// CHECK-ANSI-IR:   %0 = load i32, ptr @VA
// CHECK-ANSI-IR:   tail call void @FA(i32 %0)
// CHECK-ANSI-IR:   ret void
// CHECK-ANSI-IR: }

// CHECK-ANSI-IR: declare {{.*}}void @FA(i32 noundef)
// CHECK-ANSI-IR-NOT: declare {{.*}}void @FW(i64 noundef)

// CHECK-ANSI-IR: define {{.*}}swiftcc void @"$s5Alias1gyyF"(){{.*}}{
// CHECK-ANSI-IR: entry:
// CHECK-ANSI-IR:   store float 3.200000e+01, ptr @UIA
// CHECK-ANSI-IR:   ret void
// CHECK-ANSI-IR: }

// CHECK-UNICODE-IR: @VW = external {{(dso_local )?}}local_unnamed_addr constant i64
// CHECK-UNICODE-IR: @UIW = external {{(dso_local )?}}local_unnamed_addr global double

// CHECK-UNICODE-IR: define {{.*}}swiftcc void @"$s5Alias1fyyF"(){{.*}}{
// CHECK-UNICODE-IR: entry:
// CHECK-UNICODE-IR:   %0 = load i64, ptr @VW
// CHECK-UNICODE-IR:   tail call void @FW(i64 %0)
// CHECK-UNICODE-IR:   ret void
// CHECK-UNICODE-IR: }

// CHECK-UNICODE-IR: declare {{(dso_local )?}}void @FW(i64 noundef)
// CHECK-UNICODE-IR-NOT: declare {{(dso_local )?}}void @FA(i32 noundef)

// CHECK-UNICODE-IR: define {{.*}}swiftcc void @"$s5Alias1gyyF"(){{.*}}{
// CHECK-UNICODE-IR: entry:
// CHECK-UNICODE-IR:   store double 3.200000e+01, ptr @UIW
// CHECK-UNICODE-IR:   ret void
// CHECK-UNICODE-IR: }

func h() {
  let _ = CLOCK_MONOTONIC
}

#if INVALID
let _ = ALPHA
// CHECK-INVALID: error: global variable declaration does not bind any variables
#endif

// CHECK-FUNCTION-POINTER:      // aliased_function.getter
// CHECK-FUNCTION-POINTER-NEXT: // Isolation: unspecified
// CHECK-FUNCTION-POINTER-NEXT: sil shared [transparent] [heuristic_always_inline] @$sSC16aliased_functionyycvg : $@convention(thin) () -> @owned @Sendable @callee_guaranteed () -> () {
// CHECK-FUNCTION-POINTER:        function_ref @$sSo8functionyyFTO : $@convention(thin) () -> ()

// CHECK-FUNCTION-POINTER:      // MAFunction.getter
// CHECK-FUNCTION-POINTER-NEXT: // Isolation: unspecified
// CHECK-FUNCTION-POINTER-NEXT: sil shared [transparent] [heuristic_always_inline] @$sSC10MAFunctionyycvg : $@convention(thin) () -> @owned @Sendable @callee_guaranteed () -> () {
// CHECK-FUNCTION-POINTER:        function_ref @$sSo19main_actor_functionyyFTO : $@convention(thin) () -> ()

// CHECK-FUNCTION-POINTER:      // @nonobjc function()
// CHECK-FUNCTION-POINTER-NEXT: // Isolation: unspecified
// CHECK-FUNCTION-POINTER-NEXT: sil shared [thunk] @$sSo8functionyyFTO : $@convention(thin) () -> () {
// CHECK-FUNCTION-POINTER:        function_ref @$sSo8functionyyFTo : $@convention(c) () -> ()

// CHECK-FUNCTION-POINTER:      // @objc function()
// CHECK-FUNCTION-POINTER-NEXT: // Isolation: unspecified
// CHECK-FUNCTION-POINTER-NEXT: // clang name: function
// CHECK-FUNCTION-POINTER:      sil [asmname "function"] [clang function] @$sSo8functionyyFTo : $@convention(c) () -> ()

// CHECK-FUNCTION-POINTER:      // @nonobjc main_actor_function()
// CHECK-FUNCTION-POINTER-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-FUNCTION-POINTER-NEXT: sil shared [thunk] @$sSo19main_actor_functionyyFTO : $@convention(thin) () -> () {
// CHECK-FUNCTION-POINTER:        function_ref @$sSo19main_actor_functionyyFTo : $@convention(c) () -> ()

// CHECK-FUNCTION-POINTER:      // @objc main_actor_function()
// CHECK-FUNCTION-POINTER-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-FUNCTION-POINTER-NEXT: // clang name: main_actor_function
// CHECK-FUNCTION-POINTER:      sil [asmname "main_actor_function"] [clang main_actor_function] @$sSo19main_actor_functionyyFTo : $@convention(c) () -> ()

func test() {
    _ = aliased_function
    _ = MAFunction()
}
