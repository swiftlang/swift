// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// REQUIRES: VENDOR=apple
// REQUIRES: swift_feature_InternalImportsByDefault

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
// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/PublicImports.swift \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ -module-cache-path %t \
// RUN:   -library-level=api -verify -module-name MainLib

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
import PrivateSwift // expected-error{{private module 'PrivateSwift' is imported publicly from the public module 'MainLib'}}{{1-1=internal }}

import PublicClang
import PublicClang_Private // expected-error{{private module 'PublicClang_Private' is imported publicly from the public module 'MainLib'}}{{1-1=internal }}
import FullyPrivateClang // expected-error{{private module 'FullyPrivateClang' is imported publicly from the public module 'MainLib'}}{{1-1=internal }}
import LocalClang // expected-error{{private module 'LocalClang' is imported publicly from the public module 'MainLib'}}{{1-1=internal }}
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
// RUN: not %target-swift-frontend -typecheck %t/Empty.swift \
// RUN:   -library-level ThatsNotALibraryLevel 2>&1 \
// RUN:   | %FileCheck %s --check-prefix CHECK-ARG
// RUN: not %target-swift-frontend -typecheck %t/Empty.swift \
// RUN:   -library-level=ThatsNotALibraryLevel 2>&1 \
// RUN:   | %FileCheck %s --check-prefix CHECK-ARG
// CHECK-ARG: error: unknown library level 'ThatsNotALibraryLevel', expected one of 'api', 'spi', 'ipi', or 'other'
//--- Empty.swift

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

// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/InternalImports.swift \
// RUN:   -module-name MainLib -module-cache-path %t \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level api -verify
//--- InternalImports.swift
internal import PublicSwift
internal import PrivateSwift

internal import PublicClang
internal import PublicClang_Private
internal import FullyPrivateClang
internal import LocalClang

// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/FileprivateImports.swift \
// RUN:   -module-name MainLib -module-cache-path %t \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level api -verify
//--- FileprivateImports.swift
fileprivate import PublicSwift
fileprivate import PrivateSwift

fileprivate import PublicClang
fileprivate import PublicClang_Private
fileprivate import FullyPrivateClang
fileprivate import LocalClang

// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/PrivateImports.swift \
// RUN:   -module-name MainLib -module-cache-path %t \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level api -verify
//--- PrivateImports.swift
private import PublicSwift
private import PrivateSwift

private import PublicClang
private import PublicClang_Private
private import FullyPrivateClang
private import LocalClang

// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/ExplicitlyPublicImports.swift \
// RUN:   -module-name MainLib -module-cache-path %t \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -library-level api -verify
//--- ExplicitlyPublicImports.swift
public import PublicSwift
// expected-warning @-1 {{public import of 'PublicSwift' was not used in public declarations or inlinable code}}{{1-7=internal}}
public import PrivateSwift // expected-error{{private module 'PrivateSwift' is imported publicly from the public module 'MainLib'}}{{1-7=internal}}
// expected-warning @-1 {{public import of 'PrivateSwift' was not used in public declarations or inlinable code}}{{1-7=internal}}

public import PublicClang
// expected-warning @-1 {{public import of 'PublicClang' was not used in public declarations or inlinable code}}{{1-7=internal}}
public import PublicClang_Private // expected-error{{private module 'PublicClang_Private' is imported publicly from the public module 'MainLib'}}{{1-7=internal}}
// expected-warning @-1 {{public import of 'PublicClang_Private' was not used in public declarations or inlinable code}}{{1-7=internal}}
public import FullyPrivateClang // expected-error{{private module 'FullyPrivateClang' is imported publicly from the public module 'MainLib'}}{{1-7=internal}}
// expected-warning @-1 {{public import of 'FullyPrivateClang' was not used in public declarations or inlinable code}}{{1-7=internal}}
public import LocalClang // expected-error{{private module 'LocalClang' is imported publicly from the public module 'MainLib'}}{{1-7=internal}}
// expected-warning @-1 {{public import of 'LocalClang' was not used in public declarations or inlinable code}}{{1-7=internal}}
@_exported public import MainLib // expected-warning{{private module 'MainLib' is imported publicly from the public module 'MainLib'}}{{12-18=internal}}

// RUN: %target-swift-frontend -typecheck -sdk %t/sdk %t/ImplictlyInternalImports.swift \
// RUN:   -module-name MainLib -module-cache-path %t \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks/ \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -library-level api -verify
//--- ImplictlyInternalImports.swift
public import PublicSwift
// expected-warning @-1 {{public import of 'PublicSwift' was not used in public declarations or inlinable code}}{{1-8=}}
public import PrivateSwift // expected-error{{private module 'PrivateSwift' is imported publicly from the public module 'MainLib'}}{{1-8=}}
// expected-warning @-1 {{public import of 'PrivateSwift' was not used in public declarations or inlinable code}}{{1-8=}}
