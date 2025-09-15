// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

protocol Proto { }

struct MyStruct: Proto { }

func foo() -> some Proto {
  MyStruct()
}

// CHECK: define {{.*}}@main(
