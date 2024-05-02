// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-generated)
// RUN: %target-swift-frontend %S/Inputs/ExportedImport/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend %S/Inputs/ExportedImport/B.swift -module-name B -emit-module -emit-module-path %t/B.swiftmodule
// RUN: %target-swift-frontend %s -module-name ExportedImport -emit-module -emit-module-path \
// RUN:     %t/ExportedImport.swiftmodule -I %t -emit-symbol-graph -emit-symbol-graph-dir %t/
// RUN: %FileCheck %s --input-file %t/ExportedImport.symbols.json
// RUN: %target-swift-symbolgraph-extract -module-name ExportedImport -I %t -output-dir \
// RUN:     %t/module-generated/ -experimental-allowed-reexported-modules=A,B
// RUN: %FileCheck %s --input-file %t/module-generated/ExportedImport.symbols.json
// RUN: ls %t | %FileCheck %s --check-prefix FILES
// RUN: ls %t/module-generated/ | %FileCheck %s --check-prefix FILES

@_exported import A
@_exported import struct B.StructOne

// CHECK-NOT: InternalSymbolFromA
// CHECK-NOT: StructTwo
// CHECK-DAG: "precise":"s:1A11SymbolFromAV"
// CHECK-DAG: "precise":"s:1B9StructOneV"

// FILES-NOT: ExportedImport@A.symbols.json
