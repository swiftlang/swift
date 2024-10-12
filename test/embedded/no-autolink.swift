// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

public func staticstring() -> StaticString {
  return "hello"
}

// CHECK: !llvm.linker.options = !{}
// CHECK-NOT: -lswiftCore
