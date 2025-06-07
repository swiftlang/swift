// RUN: %target-swift-frontend -parse-as-library -primary-file %s -O -emit-ir -o - | %FileCheck %s

// REQUIRES: swift_in_compiler

// Check that IRGen doesn't crash when a global variable reference another private global.

// CHECK-LABEL: @"$s17referenced_global1bSPySiGvp" ={{.*}} global {{.*}} ptr @"$s17referenced_global1x{{.*}}"
public var b = UnsafePointer(&x)
private var x = 1
