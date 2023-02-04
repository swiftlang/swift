// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// REQUIRES: VENDOR=apple
// REQUIRES: asserts

/// Prepare the SDK.
// RUN: cp -r %S/Inputs/public-private-sdk %t/sdk
// RUN: %target-swift-frontend -emit-module -module-name PublicSwift \
// RUN:   %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/Frameworks/PublicSwift.framework/Modules/PublicSwift.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -emit-module -module-name PrivateSwift \
// RUN:   %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/source.swift \
// RUN:   -o %t/sdk/System/Library/PrivateFrameworks/PrivateSwift.framework/Modules/PrivateSwift.swiftmodule/%target-swiftmodule-name

/// Expect errors when building a public client.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/PublicImports.swift \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ -module-cache-path %t \
// RUN:   -library-level api -verify -module-name MainLib

/// Expect no errors when building an SPI client.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/PublicImports.swift \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ -module-cache-path %t \
// RUN:   -library-level spi -module-name MainLib

/// The driver should also accept the flag and pass it along.
// RUN: %target-swiftc_driver -typecheck -sdk %t/sdk %t/PublicImports.swift \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ -module-cache-path %t \
// RUN:   -library-level spi -module-name MainLib

/// Expect no errors when building a client with some other library level.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/PublicImports.swift \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ -module-cache-path %t \
// RUN:   -module-name MainLib
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/PublicImports.swift \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ -module-cache-path %t \
// RUN:   -library-level other -module-name MainLib
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/PublicImports.swift \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ -module-cache-path %t \
// RUN:   -library-level ipi -module-name MainLib
//--- PublicImports.swift
import PublicSwift
import PrivateSwift // expected-error{{private module 'PrivateSwift' is imported publicly from the public module 'MainLib'}}

import PublicClang
import PublicClang_Private // expected-error{{private module 'PublicClang_Private' is imported publicly from the public module 'MainLib'}}
import FullyPrivateClang // expected-error{{private module 'FullyPrivateClang' is imported publicly from the public module 'MainLib'}}
import LocalClang // expected-error{{private module 'LocalClang' is imported publicly from the public module 'MainLib'}}
@_exported import MainLib // expected-warning{{private module 'MainLib' is imported publicly from the public module 'MainLib'}}

/// Expect no errors with implementation-only imports.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/ImplOnlyImports.swift \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ -module-cache-path %t \
// RUN:   -library-level api -D IMPL_ONLY_IMPORTS
//--- ImplOnlyImports.swift

@_implementationOnly import PrivateSwift
@_implementationOnly import PublicClang_Private
@_implementationOnly import FullyPrivateClang
@_implementationOnly import LocalClang

/// Expect no errors with spi-only imports.
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/SpiOnlyImports.swift \
// RUN:   -experimental-spi-only-imports -module-cache-path %t \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level api
//--- SPIOnlyImports.swift

@_spiOnly import PrivateSwift
@_spiOnly import PublicClang_Private
@_spiOnly import FullyPrivateClang
@_spiOnly import LocalClang

/// Test error message on an unknown library level name.
// RUN: not %target-swift-frontend -typecheck %s -library-level ThatsNotALibraryLevel 2>&1 \
// RUN:   | %FileCheck %s --check-prefix CHECK-ARG
// CHECK-ARG: error: unknown library level 'ThatsNotALibraryLevel', expected one of 'api', 'spi', 'ipi', or 'other'

/// Expect no errors in swiftinterfaces.
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) \
// RUN: -sdk %t/sdk -module-cache-path %t -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -I %t -module-name Client

//--- Client.private.swiftinterface
// swift-interface-format-version: 1.0
// swift-compiler-version: Swift version 5.8-dev effective-4.1.50
// swift-module-flags: -swift-version 4 -module-name Client -library-level api

import PublicSwift
import PrivateSwift
import PublicClang
import PublicClang_Private
import FullyPrivateClang
import LocalClang
@_exported import MainLib
