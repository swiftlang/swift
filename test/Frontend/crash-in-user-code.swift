
// RUN: echo %s > %t.filelist.txt
// RUN: not --crash %target-swift-frontend -interpret -filelist %t.filelist.txt 2>&1 | %FileCheck %s

// REQUIRES: executable_test

// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=watchos

// CHECK: Stack dump:
// CHECK-NEXT: Program arguments:
// CHECK-NEXT: Swift version
// CHECK-NEXT: Contents of {{.*}}.filelist.txt:
// CHECK-NEXT: ---
// CHECK-NEXT: crash-in-user-code.swift
// CHECK-NEXT: ---
// CHECK-NEXT: While running user code "{{.*}}crash-in-user-code.swift"

let x: Int? = nil
x!
