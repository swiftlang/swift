/// Test that the AliasModuleNames mode avoids ambiguities in swiftinterfaces

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name ExportedLib \
// RUN:     -swift-version 5 -enable-library-evolution \
// RUN:     -o %t/ExportedLib.swiftmodule \
// RUN:     -emit-module-interface-path %t/ExportedLib.swiftinterface \
// RUN:     %t/ExportedLib.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/ExportedLib.swiftinterface)

// RUN: %target-swift-frontend -emit-module -module-name AmbiguousLib \
// RUN:     -swift-version 5 -enable-library-evolution \
// RUN:     -o %t/AmbiguousLib.swiftmodule \
// RUN:     -emit-module-interface-path %t/AmbiguousLib.swiftinterface \
// RUN:     %t/AmbiguousLib.swift -I%t
// RUN: %target-swift-typecheck-module-from-interface(%t/AmbiguousLib.swiftinterface) -I%t

// RUN: %target-swift-frontend -emit-module -module-name AmbiguousClientName \
// RUN:     -swift-version 5 -enable-library-evolution \
// RUN:     -o %t/AmbiguousClientName.swiftmodule \
// RUN:     -emit-module-interface-path %t/AmbiguousClientName.swiftinterface \
// RUN:     %t/AmbiguousClientName.swift -I%t \
// RUN:     -alias-module-names-in-module-interface
// RUN: %target-swift-typecheck-module-from-interface(%t/AmbiguousClientName.swiftinterface) -I%t

// RUN: %target-swift-frontend -emit-module -module-name OverlayClient \
// RUN:     -swift-version 5 -enable-library-evolution \
// RUN:     -o %t/OverlayClient.swiftmodule \
// RUN:     -emit-module-interface-path %t/OverlayClient.swiftinterface \
// RUN:     %t/OverlayClient.swift -I%t \
// RUN:     -alias-module-names-in-module-interface
// RUN: %target-swift-typecheck-module-from-interface(%t/OverlayClient.swiftinterface) -I%t

//--- module.modulemap
module AmbiguousClientName {
    header "AmbiguousClientName.h"
}

module SomeClangModule {
    header "SomeClangModule.h"
}

//--- AmbiguousClientName.h
struct UnderlyingType {};
void underlyingFunc() {}

//--- SomeClangModule.h

struct CType {};

//--- ExportedLib.swift

public struct ExportedStruct {}

//--- AmbiguousLib.swift

@_exported import ExportedLib

// 1. AmbiguousLib defined a type named AmbiguousLib
public struct AmbiguousLib {
    public struct Nested {}
}

// 2. A lib defines a type of the same name as a client's module
public struct AmbiguousClientName {
}

//--- AmbiguousClientName.swift

@_exported import AmbiguousClientName
import AmbiguousLib
import SomeClangModule

public struct SomeType {
    @inlinable
    public func inlinableFunc() {
        var x: AmbiguousClientName
    }
}

public func refToLocalType(_ a: SomeType) {}
public func refToNestedInLib(_ a: AmbiguousLib.Nested) {}

public func refToStdlib(_ a: Swift.Int) {}
public func refToUnderlying(_ a: UnderlyingType) {}
public func refToC(_ a: CType) {}

public func refToReexport(_ a: ExportedStruct) {}

//--- OverlayClient.swift

import AmbiguousClientName

public func refToImportedType(_ a: SomeType) {}
public func refToImportedUnderlying(_ a: UnderlyingType) {}
