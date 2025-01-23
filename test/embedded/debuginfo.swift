// RUN: %target-swift-frontend -g -emit-ir %s -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -g -O -emit-ir %s -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -g -Osize -emit-ir %s -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -O -emit-ir %s -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -Osize -emit-ir %s -enable-experimental-feature Embedded

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func foo<T>(_ array: inout [T]) {
    array.withUnsafeMutableBytes {
        $0[0] = 0
    }
}

func foo2<T>(_ array: inout [T]) {
    array.withUnsafeMutableBytes {
        $0[0] = 0
    }
}

public func test() {
    var a: [UInt8] = [1, 2, 3]
    foo(&a)
    foo2(&a)
}
