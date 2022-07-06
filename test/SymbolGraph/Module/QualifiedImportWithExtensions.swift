// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/QualifiedImportWithExtensions/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend %s -module-name QualifiedImportWithExtensions -emit-module -emit-module-path /dev/null -I %t -emit-symbol-graph -emit-symbol-graph-dir %t/
// RUN: %FileCheck %s --input-file %t/QualifiedImportWithExtensions.symbols.json
// RUN: %FileCheck %s --input-file %t/QualifiedImportWithExtensions@A.symbols.json --check-prefix EXT

@_exported import struct A.StructOne
import A

// An extension to a type that's been re-exported with a qualified import should appear in the
// main symbol graph.
extension A.StructOne {
    public struct ExtendedStruct {}
}

// An extension to a type that's _not_ been re-exported should still appear in the extension symbol
// graph, even if a different type from that module has been re-exported.
extension A.StructTwo {
    public struct InnerStruct {}
}

// Make sure that the `ExtendedStruct` type is present, and also that `StructOne`'s initializer is
// present as well. The `InnerStruct` type should not be present since it's extending a type
// that's not being re-exported.

// CHECK-NOT: InnerStruct
// CHECK-DAG: ExtendedStruct
// CHECK-DAG: "precise":"s:1A9StructOneVACycfc"

// EXT: InnerStruct
// EXT-NOT: ExtendedStruct
