// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/includes)

// Build external Swift library/module to also check conformances to external protocols
// RUN: %target-build-swift %S/Inputs/swiftmodules/testModB.swift -parse-as-library -emit-module -emit-library -module-name testModB -o %t/includes/testModB.o

// RUN: %target-build-swift %s -parse-as-library -target %target-cpu-apple-macosx10.15 -I %t/includes -emit-module -emit-library -module-name AssociatedTypeRequirements -o %t/AssociatedTypeRequirements %t/includes/testModB.o

// RUN: %target-swift-reflection-dump -binary-filename %t/AssociatedTypeRequirements -binary-filename %platform-module-dir/%target-library-name(swiftCore) | %FileCheck %s

// CHECK: ASSOCIATED TYPES:
// CHECK-NEXT: =============
// CHECK-NEXT: - AssociatedTypeRequirements.Foo : AssociatedTypeRequirements.myProto
// CHECK-NEXT: typealias PerformReturn = opaque type symbolic reference
// CHECK-NEXT: opaque type symbolic reference
// CHECK-NEXT: (struct AssociatedTypeRequirements.Bar)

// CHECK: opaque type conformance requirements:
// CHECK-DAG: AssociatedTypeRequirements.protoA
// CHECK-DAG: AssociatedTypeRequirements.protoB
// CHECK-DAG: testModB.testModBProtocol

import testModB

public protocol myProto {
    associatedtype PerformReturn
    func perform() -> PerformReturn
}
public protocol protoA {}
public protocol protoB {}

public struct Bar : protoA, protoB, testModBProtocol {}

public struct Foo : myProto {
    public func perform() -> some protoA & protoB & testModBProtocol { return baz() }
}

private func baz() -> some protoA & protoB & testModBProtocol { return Bar() }
