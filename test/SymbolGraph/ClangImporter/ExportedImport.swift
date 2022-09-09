// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/ExportedImport/ObjcProperty.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t/ObjcProperty.framework/Modules/ObjcProperty.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name ObjcProperty %S/Inputs/ExportedImport/A.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t/ExportedImport.swiftmodule -F %t -module-name ExportedImport %s -emit-symbol-graph -emit-symbol-graph-dir %t
// wait: false
// RUN: %FileCheck %s --input-file %t/ExportedImport.symbols.json

// REQUIRES: objc_interop

// CHECK-DAG: "precise":"s:So11ClangStructa12ObjcPropertyE05InnerB0V"

@_exported import ObjcProperty
