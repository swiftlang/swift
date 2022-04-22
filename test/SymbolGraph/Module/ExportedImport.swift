// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/ExportedImport/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend %s -module-name ExportedImport -emit-module -emit-module-path /dev/null -I %t -emit-symbol-graph -emit-symbol-graph-dir %t/
// RUN: %FileCheck %s --input-file %t/ExportedImport.symbols.json
// RUN: ls %t | %FileCheck %s --check-prefix FILES

@_exported import A

// CHECK: "precise":"s:1A11SymbolFromAV"
// CHECK-NOT: InternalSymbolFromA

// FIXME: Symbols from `@_exported import` do not get emitted when using swift-symbolgraph-extract
// This is tracked by https://bugs.swift.org/browse/SR-15921.

// FILES-NOT: ExportedImport@A.symbols.json
