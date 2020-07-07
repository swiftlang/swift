// RUN: %swift %s -emit-ir | %FileCheck %s
// RUN: %swift -target %target-cpu-apple-macosx10.51 %s -emit-ir | %FileCheck -check-prefix=CHECK-SPECIFIC-MAC-10-X %s
// RUN: %swift -target %target-cpu-apple-darwin55 %s -emit-ir | %FileCheck -check-prefix=CHECK-DARWIN-OVER-11 %s

// REQUIRES: OS=macosx

// CHECK: target triple = "{{.*}}-apple-macosx{{[0-9][0-9]}}.
// CHECK-SPECIFIC-MAC-10-X: target triple = "{{.*}}-apple-macosx10.51.0"
// CHECK-DARWIN-OVER-11: target triple = "{{.*}}-apple-macosx46.0.0"

public func anchor() {}
anchor()

