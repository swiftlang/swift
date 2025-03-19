// RUN: %target-swift-frontend %s -mergeable-symbols    -enable-experimental-feature Embedded -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend %s -mergeable-symbols -O -enable-experimental-feature Embedded -emit-ir | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

public func foo1() { }

func foo2() { }

// CHECK: foo1
// CHECK-NOT: foo2
