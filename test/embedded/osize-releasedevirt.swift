// RUN: %target-swift-emit-ir -Osize %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

public func foo() {
    bar([42])
}

func bar(_: UnsafePointer<UInt?>) {
}

// CHECK: define {{.*}}@main(
