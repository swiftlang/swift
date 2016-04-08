// RUN: %target-swift-frontend -primary-file %s -O -emit-ir | FileCheck %s

func foo() {}

// CHECK: @__swift_reflection_version = linkonce_odr constant i32

