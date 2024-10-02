// Tests the `@_PlaygroundTransformed` function annotation
// with Playground transforms enabled but without the
// `SelectiveTransform` playground option enabled.
// The annotation shouldn't have any affect in this case,
// all the code will be transformed.
//
// REQUIRES: executable_test
//
// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
//
// - Transformed, No SelectiveTransform
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main1b -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main1b
// RUN: %target-run %t/main1b | %FileCheck %s
//
// - Transformed + PC-Macro, No SelectiveTransform
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main2b -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main2b
// RUN: %target-run %t/main2b | %FileCheck %s

import PlaygroundSupport

// Non-annotated function
func a(w: Int, h: Int) -> Int {
    return w * h
}
let area = a(w: 10, h: 20)

// CHECK:      {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[w='10']
// CHECK-NEXT: {{.*}} __builtin_log[h='20']
// CHECK-NEXT: {{.*}} __builtin_log[='200']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[area='200']

// Annotated function
@_PlaygroundTransformed
func f(x: Int, _ y: Float = 7.62) -> Int {
    return x + Int(y)
}
let foo = f(x: 42)

// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[x='42']
// CHECK-NEXT: {{.*}} __builtin_log[y='7.62']
// CHECK-NEXT: {{.*}} __builtin_log[='49']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[foo='49']
