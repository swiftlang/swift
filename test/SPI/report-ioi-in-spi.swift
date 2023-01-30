// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -module-name Lib -emit-module-path %t/Lib.swiftmodule \
// RUN:   -swift-version 5

/// Use of IOI types in SPI signatures is an error with -experimental-spi-only-imports
// RUN: %target-swift-frontend -emit-module %t/ClientSPIOnlyMode.swift -I %t \
// RUN:   -swift-version 5 -verify \
// RUN:   -experimental-spi-only-imports

/// Use of IOI types in SPI signatures is a warning without -experimental-spi-only-imports
// RUN: %target-swift-frontend -emit-module %t/ClientDefaultMode.swift -I %t \
// RUN:   -swift-version 5 -verify

/// This is a warning in swiftinterfaces
// R UN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) \
// R UN:   -I %t -module-name Client

//--- Lib.swift

public struct IOIStruct {
    public init() {}
}

//--- ClientSPIOnlyMode.swift

@_implementationOnly import Lib

@_spi(X) public func spiClient(s: IOIStruct) -> IOIStruct { // expected-error 2 {{cannot use struct 'IOIStruct' here; 'Lib' has been imported as implementation-only}}
    return IOIStruct()
}

@_spi(X) @inlinable public func inlinableClient(s: IOIStruct) -> IOIStruct { // expected-error 2 {{struct 'IOIStruct' cannot be used in an '@inlinable' function because 'Lib' was imported implementation-only}}
    return IOIStruct() // expected-error {{struct 'IOIStruct' cannot be used in an '@inlinable' function because 'Lib' was imported implementation-only}}
    // expected-error @-1 {{initializer 'init()' cannot be used in an '@inlinable' function because 'Lib' was imported implementation-only}}
}

//--- ClientDefaultMode.swift

@_implementationOnly import Lib

@_spi(X) public func spiClient(s: IOIStruct) -> IOIStruct { // expected-warning 2 {{cannot use struct 'IOIStruct' here; 'Lib' has been imported as implementation-only}}
    return IOIStruct()
}

@_spi(X) @inlinable public func inlinableClient(s: IOIStruct) -> IOIStruct { // expected-error 2 {{struct 'IOIStruct' cannot be used in an '@inlinable' function because 'Lib' was imported implementation-only}}
    return IOIStruct() // expected-error {{struct 'IOIStruct' cannot be used in an '@inlinable' function because 'Lib' was imported implementation-only}}
    // expected-error @-1 {{initializer 'init()' cannot be used in an '@inlinable' function because 'Lib' was imported implementation-only}}
}

//--- Client.private.swiftinterface

// swift-interface-format-version: 1.0
// swift-compiler-version: Swift version 5.8-dev effective-4.1.50
// swift-module-flags: -swift-version 4 -module-name Client
@_implementationOnly import Lib

@_spi(X) public func spiClient() -> IOIStruct { fatalError() }
