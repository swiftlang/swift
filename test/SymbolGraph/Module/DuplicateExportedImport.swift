// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-generated)
// RUN: %target-swift-frontend %S/Inputs/DuplicateExportedImport/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend %s -module-name DuplicateExportedImport -emit-module -emit-module-path \
// RUN:     %t/DuplicateExportedImport.swiftmodule -I %t -emit-symbol-graph -emit-symbol-graph-dir %t/
// RUN: %FileCheck %s --input-file %t/DuplicateExportedImport.symbols.json
// RUN: %target-swift-symbolgraph-extract -module-name DuplicateExportedImport -I %t -output-dir %t/module-generated/ \
// RUN:     -experimental-allowed-reexported-modules=A
// RUN: %FileCheck %s --input-file %t/module-generated/DuplicateExportedImport.symbols.json

// REQUIRES: asserts

// CHECK-COUNT-1: "precise":"s:1A8ClassTwoC"

@_exported import A
@_exported import class A.ClassTwo
