// RUN: %target-swift-emit-ir -Osize %s -enable-experimental-feature Embedded | %FileCheck %s

public func foo() {
    bar([42])
}

func bar(_: UnsafePointer<UInt?>) {
}

// CHECK: define {{.*}}@main(
