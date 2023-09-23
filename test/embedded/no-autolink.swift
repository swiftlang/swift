// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

public func staticstring() -> StaticString {
  return "hello"
}

// CHECK: !llvm.linker.options = !{}
// CHECK-NOT: -lswiftCore
