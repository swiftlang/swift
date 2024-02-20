/// Test conflicting imports modifiers from the same line.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Generate dependencies.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -module-name Lib -emit-module-path %t/Lib.swiftmodule \
// RUN:   -swift-version 5 -enable-library-evolution

/// Build clients.
// RUN: %target-swift-frontend -typecheck %t/SPIOnly_Exported.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports -verify
// RUN: %target-swift-frontend -typecheck %t/Exported_SPIOnly.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports -verify
// RUN: %target-swift-frontend -typecheck %t/SPIOnly_IOI.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports -verify
// RUN: %target-swift-frontend -typecheck %t/SPIOnly_IOI_Exported.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports -verify

//--- Lib.swift
// Empty source file for import.

//--- SPIOnly_Exported.swift
@_spiOnly @_exported import Lib // expected-error {{module 'Lib' cannot be both exported and SPI only}}

//--- Exported_SPIOnly.swift
@_exported @_spiOnly import Lib // expected-error {{module 'Lib' cannot be both exported and SPI only}}

//--- SPIOnly_IOI.swift
@_spiOnly @_implementationOnly import Lib // expected-error {{module 'Lib' cannot be both implementation-only and SPI only}}

//--- Exported_IOI.swift
@_exported @_implementationOnly import Lib // expected-error {{module 'Lib' cannot be both exported and implementation-only}}

//--- SPIOnly_IOI_Exported.swift
@_spiOnly @_implementationOnly @_exported import Lib // expected-error {{module 'Lib' cannot be both exported and implementation-only}}

/// Access levels on imports
// RUN: %target-swift-frontend -typecheck %t/Public_Exported.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports -verify
// RUN: %target-swift-frontend -typecheck %t/Package_Exported.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports -verify
// RUN: %target-swift-frontend -typecheck %t/Internal_Exported.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports -verify
// RUN: %target-swift-frontend -typecheck %t/Fileprivate_Exported.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports -verify
// RUN: %target-swift-frontend -typecheck %t/Private_Exported.swift -I %t -verify \
// RUN:   -experimental-spi-only-imports -verify

//--- Public_Exported.swift
@_exported public import Lib

//--- Package_Exported.swift
@_exported package import Lib // expected-error {{'@_exported' is incompatible with 'package'; it can only be applied to public imports}}

//--- Internal_Exported.swift
@_exported internal import Lib // expected-error {{'@_exported' is incompatible with 'internal'; it can only be applied to public imports}}

//--- Fileprivate_Exported.swift
@_exported fileprivate import Lib // expected-error {{'@_exported' is incompatible with 'fileprivate'; it can only be applied to public imports}}

//--- Private_Exported.swift
@_exported private import Lib // expected-error {{'@_exported' is incompatible with 'private'; it can only be applied to public imports}}
