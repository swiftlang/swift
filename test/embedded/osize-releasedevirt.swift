// RUN: %target-swift-emit-ir -Osize %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=windows-msvc

public func foo() {
    bar([42])
}

func bar(_: UnsafePointer<UInt?>) {
}

// CHECK: define {{.*}}@main(
