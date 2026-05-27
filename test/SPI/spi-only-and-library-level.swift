// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -module-name Lib -emit-module-path %t/Lib.swiftmodule \
// RUN:   -swift-version 5
// RUN: %target-swift-frontend -emit-module %t/IPILib.swift -I %t \
// RUN:   -module-name IPILib -emit-module-path %t/IPILib.swiftmodule \
// RUN:   -swift-version 5 -library-level ipi
// RUN: %target-swift-frontend -emit-module %t/APILib.swift -I %t \
// RUN:   -swift-version 5 -verify -verify-ignore-unrelated \
// RUN:   -experimental-spi-only-imports \
// RUN:   -library-level api \
// RUN:   -require-explicit-availability=ignore
// RUN: %target-swift-frontend -emit-module %t/SPILib.swift -I %t \
// RUN:   -swift-version 5 -verify -verify-ignore-unrelated \
// RUN:   -experimental-spi-only-imports \
// RUN:   -library-level spi
// RUN: %target-swift-frontend -emit-module %t/OtherLib.swift -I %t \
// RUN:   -swift-version 5 -verify -verify-ignore-unrelated \
// RUN:   -experimental-spi-only-imports

// RUN: %target-swift-frontend -emit-module %t/APISpiOnlyIPI.swift -I %t \
// RUN:   -swift-version 5 -verify -verify-ignore-unrelated \
// RUN:   -experimental-spi-only-imports \
// RUN:   -library-level api \
// RUN:   -require-explicit-availability=ignore
// RUN: %target-swift-frontend -emit-module %t/SPISpiOnlyIPI.swift -I %t \
// RUN:   -swift-version 5 -verify -verify-ignore-unrelated \
// RUN:   -experimental-spi-only-imports \
// RUN:   -library-level spi

// RUN: %target-swift-frontend -emit-module %t/IPISpiOnlyIPI.swift -I %t \
// RUN:   -swift-version 5 -verify -verify-ignore-unrelated \
// RUN:   -experimental-spi-only-imports \
// RUN:   -library-level ipi

//--- Lib.swift

public struct LibStruct {}

//--- IPILib.swift
public struct IPIStruct {}

//--- APILib.swift

@_spiOnly import Lib

public func publicClient() -> LibStruct { fatalError() } // expected-error {{cannot use struct 'LibStruct' here; 'Lib' was imported for SPI only}}
@_spi(X) public func spiClient() -> LibStruct { fatalError() }

//--- SPILib.swift

@_spiOnly import Lib

public func publicClient() -> LibStruct { fatalError() }
@_spi(X) public func spiClient() -> LibStruct { fatalError() }

//--- OtherLib.swift

@_spiOnly import Lib

public func publicClient() -> LibStruct { fatalError() } // expected-error {{cannot use struct 'LibStruct' here; 'Lib' was imported for SPI only}}
@_spi(X) public func spiClient() -> LibStruct { fatalError() }

//--- APISpiOnlyIPI.swift

@_spiOnly import IPILib // expected-error {{Project internal module 'IPILib' cannot be imported publicly from non-internal module 'APISpiOnlyIPI'}}

//--- SPISpiOnlyIPI.swift

@_spiOnly import IPILib // expected-error {{Project internal module 'IPILib' cannot be imported publicly from non-internal module 'SPISpiOnlyIPI'}}

//--- IPISpiOnlyIPI.swift

@_spiOnly import IPILib
