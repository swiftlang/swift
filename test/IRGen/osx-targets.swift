// RUN: %swift %s -emit-ir | %FileCheck %s
// RUN: %swift -target %target-cpu-apple-macosx10.51 %s -emit-ir | %FileCheck -check-prefix=CHECK-SPECIFIC %s
// RUN: %swift -target %target-cpu-apple-darwin55 %s -emit-ir | %FileCheck -check-prefix=CHECK-SPECIFIC %s

// REQUIRES: OS=macosx

// CHECK: target triple = "{{.*}}-apple-macosx10.
// CHECK-SPECIFIC: target triple = "{{.*}}-apple-macosx10.51.0"

public func anchor() {}
anchor()

