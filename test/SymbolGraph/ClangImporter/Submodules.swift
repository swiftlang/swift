// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/Submodules -emit-module-path %t/Submodules.swiftmodule -enable-objc-interop -module-name Submodules %s -emit-symbol-graph -emit-symbol-graph-dir %t

// REQUIRES: objc_interop

// Don't crash when a module declared an `@_exported import` for a Clang non-top-level module.

@_exported import Mixed
@_exported import Mixed.Submodule

public func someFunc() {}
