// RUN: %target-swift-emit-ir -Osize %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

public func foo<T>(n: T) {
    bar(n: 42)
}

private func bar<T>(n: T) {
    baz(n: 42)
}

public func baz<T>(n: T) {
    let x: ContiguousArray<Int> = .init(repeating: 0, count: 1)
}

// CHECK: define {{.*}}@{{_*}}main{{.*}}(
