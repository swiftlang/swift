// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface) -module-name Lib

//--- Lib.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Lib -enable-library-evolution

// @_spi_available(macOS 10.15, *) gets rewritten to @available(macOS, unavailable)
// in the public swiftinterface; the ODI check must not fire here.
@available(macOS, unavailable)
@_originallyDefinedIn(module: "original", macOS 10.15)
public func unavailableWithODI() {}
