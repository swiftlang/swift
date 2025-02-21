// RUN: %empty-directory(%t)

// Don't crash when a module declared an `@_exported import` for a Clang non-top-level module.

// RUN: cp -r %S/Inputs/EmitWhileBuilding/EmitWhileBuilding.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module-path %t/EmitWhileBuilding.framework/Modules/EmitWhileBuilding.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name EmitWhileBuilding -disable-objc-attr-requires-foundation-module %S/EmitWhileBuilding.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/Submodules -emit-module-path %t/Submodules.swiftmodule -enable-objc-interop -module-name Submodules -F %t %s -emit-symbol-graph -emit-symbol-graph-dir %t

// When extracting symbol graphs for a Clang submodule, make sure that (1) the program passes
// without crashing, and (2) that the symbols correctly appear in the symbol graph.

// RUN: %empty-directory(%t)
// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name Mixed.Submodule -I %S/Inputs/Submodules -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/Mixed.Submodule.symbols.json --check-prefix SUBMODULE

// REQUIRES: objc_interop

@_exported import Mixed
@_exported import Mixed.Submodule

@_exported import EmitWhileBuilding

public func someFunc() {}

// SUBMODULE-DAG: "name": "Mixed.Submodule"
// SUBMODULE-DAG: "precise": "c:@innerVar"
