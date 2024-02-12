// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/DuplicateExportedImport/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend %s -module-name DuplicateExportedImport -emit-module -emit-module-path /dev/null -I %t -emit-symbol-graph -emit-symbol-graph-dir %t/
// RUN: %FileCheck %s --input-file %t/DuplicateExportedImport.symbols.json

// REQUIRES: asserts

// CHECK-COUNT-1: "precise":"s:1A8ClassTwoC"

@_exported import A
@_exported import class A.ClassTwo
