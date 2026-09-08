// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -target %target-cpu-apple-macos15.0 -module-name EmbeddedGenerics -enable-experimental-feature Embedded -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/embedded-generics.h
// RUN: %FileCheck %s --implicit-check-not='protocol descriptor for Hashable' --implicit-check-not='getConformanceWitnessTable<' < %t/embedded-generics.h
// RUN: %check-interop-cxx-header-in-clang(%t/embedded-generics.h -target %target-cpu-apple-macos15.0 -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

public func requiresHashable<Value: Hashable>(_ value: Value) {}

public struct HashableBox<Value: Hashable> {
    public let value: Value

    public init(_ value: Value) {
        self.value = value
    }
}

// Runtime protocol-conformance lookup is not available in Embedded Swift.
// CHECK: class HashableBox { } SWIFT_UNAVAILABLE_MSG(
// CHECK: // Unavailable in C++: Swift global function 'requiresHashable
