// RUN: %target-swift-emit-ir -Osize %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func foo() {
    bar([42])
}

func bar(_: UnsafePointer<UInt?>) {
}

// CHECK: define {{.*}}@main(
