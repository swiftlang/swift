// REQUIRES: OS=macosx
// UNSUPPORTED: CPU=arm64e
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/includes)

// Build external Swift library/module to also check conformances to external protocols
// RUN: %target-build-swift %S/Inputs/swiftmodules/testModB.swift -parse-as-library -target %target-cpu-apple-macosx10.15 -emit-module -emit-library -module-name testModB -o %t/includes/testModB.o

// RUN: %target-build-swift %s -parse-as-library -target %target-cpu-apple-macosx10.15 -I %t/includes -emit-module -emit-library -module-name AssociatedTypeRequirements -o %t/AssociatedTypeRequirements %t/includes/testModB.o

// RUN: %target-swift-reflection-dump %t/AssociatedTypeRequirements | %FileCheck %s

// This test is useless and was just relying on a bug where we didn't substitute away
// known opaque return types in non-WMO mode.

// CHECK: ASSOCIATED TYPES:
// CHECK: - AssociatedTypeRequirements.Foo : AssociatedTypeRequirements.myProto
// CHECK-NEXT: typealias PerformReturn = AssociatedTypeRequirements.Bar<testModB.testModBStruct, Swift.Float>

import testModB

public protocol myProto {
    associatedtype PerformReturn
    func perform() -> PerformReturn
}
public protocol protoA<T> {
    associatedtype T
}
public protocol protoB<K> {
    associatedtype K
}

public struct Bar<M, N> : protoA, protoB, testModBProtocol {
    public typealias T = M
    public typealias K = N
}

public struct Foo : myProto {
    public func perform() -> some protoA<testModBStruct> & protoB<Float> & testModBProtocol { return baz() }
}

private func baz() -> some protoA<testModBStruct> & protoB<Float> & testModBProtocol { return Bar<testModBStruct, Float>() }
