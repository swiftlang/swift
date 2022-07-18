// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/EmitWhileBuilding/EmitWhileBuilding.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module-path %t/EmitWhileBuilding.framework/Modules/EmitWhileBuilding.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name EmitWhileBuilding -disable-objc-attr-requires-foundation-module %s %S/Inputs/EmitWhileBuilding/Extra.swift -emit-symbol-graph -emit-symbol-graph-dir %t
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json --check-prefix BASE
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding@Swift.symbols.json --check-prefix EXTENSION

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name EmitWhileBuilding -F %t -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json --check-prefix BASE
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding@Swift.symbols.json --check-prefix EXTENSION

// REQUIRES: objc_interop

// ensure that the symbol `String.Foo.bar` does not appear in the base module's symbol graph

// BASE-NOT:     "s:SS17EmitWhileBuildingE3FooO3baryA2CmF",
// EXTENSION:    "s:SS17EmitWhileBuildingE3FooO3baryA2CmF",

public extension String {
    enum Foo {
        case bar
    }
}

