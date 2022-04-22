// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/EmitWhileBuilding/EmitWhileBuilding.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module-path %t/EmitWhileBuilding.framework/Modules/EmitWhileBuilding.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name EmitWhileBuilding -disable-objc-attr-requires-foundation-module %S/EmitWhileBuilding.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/Submodules -emit-module-path %t/Submodules.swiftmodule -enable-objc-interop -module-name Submodules -F %t %s -emit-symbol-graph -emit-symbol-graph-dir %t

// REQUIRES: objc_interop

// Don't crash when a module declared an `@_exported import` for a Clang non-top-level module.

@_exported import Mixed
@_exported import Mixed.Submodule

@_exported import EmitWhileBuilding

public func someFunc() {}
