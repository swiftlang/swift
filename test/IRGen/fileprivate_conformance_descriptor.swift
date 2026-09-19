// RUN: %target-swift-frontend -emit-ir -primary-file %s %S/Inputs/fileprivate_conformance_descriptor_other.swift -module-name TestMod -target %target-swift-5.10-abi-triple | %FileCheck %s

// REQUIRES: concurrency

// Verify that the conformance descriptor for the file-private protocol is
// emitted as an external declaration (not internal) so the linker can resolve
// it from the other object file.

// CHECK: @"$s7TestMod5OuterC5InnerVAA8_Private{{[^"]*}}Mc" = external

public func exercise() async throws {
    for try await x in Outer.Inner() {
        _ = x
    }
}
