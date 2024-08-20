// Binary swiftmodules are restricted to a channel when set in the compiler.

// RUN: %empty-directory(%t/cache)
// RUN: %empty-directory(%t/build)
// RUN: split-file %s %t --leading-lines

//--- Lib.swift
public func foo() {}

/// Build Lib as a resilient and non-resilient swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 \
// RUN:   -o %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -module-name ResilientLib -enable-library-evolution \
// RUN:   -emit-module-interface-path %t/build/ResilientLib.swiftinterface
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 \
// RUN:   -o %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -module-name NonResilientLib

/// Build a channel restricted Lib.
// RUN: env SWIFT_FORCE_SWIFTMODULE_CHANNEL=restricted-channel \
// RUN:   %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 \
// RUN:   -o %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -module-name ChannelLib -enable-library-evolution


/// 2. Test importing the non-resilient no-channel library from a channel compiler.
//--- NonResilientClient.swift
import NonResilientLib // expected-error {{the binary module for 'NonResilientLib' was compiled for '', it cannot be imported by the current compiler which is aligned with 'restricted-channel'. Binary module loaded from path:}}
foo()

/// Building a NonResilientLib client should reject the import for a tagged compiler
// RUN: env SWIFT_FORCE_SWIFTMODULE_CHANNEL=restricted-channel \
// RUN:   %target-swift-frontend -typecheck %t/NonResilientClient.swift \
// RUN:   -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -verify -verify-ignore-unknown


/// 3. Test importing the resilient no-channel library.
//--- ResilientClient.swift
import ResilientLib // expected-reject-error {{the binary module for 'ResilientLib' was compiled for '', it cannot be imported by the current compiler which is aligned with 'restricted-channel'. Binary module loaded from path:}}
 // expected-rebuild-remark @-1 {{rebuilding module 'ResilientLib' from interface}}
 // expected-rebuild-note @-2 {{compiled module is out of date}}
 // expected-rebuild-note @-3 {{compiled for a different distribution channel}}
foo()

/// ResilientLib client should rebuild from swiftinterface when available.
// RUN: env SWIFT_FORCE_SWIFTMODULE_CHANNEL=restricted-channel \
// RUN:   %target-swift-frontend -typecheck %t/ResilientClient.swift \
// RUN:   -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -verify -verify-additional-prefix rebuild- -Rmodule-interface-rebuild

/// Importing for a different channel should rebuild without cache collision.
// RUN: env SWIFT_FORCE_SWIFTMODULE_CHANNEL=other-channel \
// RUN:   %target-swift-frontend -typecheck %t/ResilientClient.swift \
// RUN:   -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -verify -verify-additional-prefix rebuild- -Rmodule-interface-rebuild
// RUN: env SWIFT_FORCE_SWIFTMODULE_CHANNEL=restricted-channel \
// RUN:   %target-swift-frontend -typecheck %t/ResilientClient.swift \
// RUN:   -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -verify -Rmodule-interface-rebuild

// RUN: rm %t/build/ResilientLib.swiftinterface
// RUN: %empty-directory(%t/cache)

/// Building a ResilientLib client should succeed in no-channel / dev mode.
// RUN: %target-swift-frontend -typecheck %t/ResilientClient.swift \
// RUN:   -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache

/// Building a ResilientLib client should reject the import for a channel compiler.
// RUN: env SWIFT_FORCE_SWIFTMODULE_CHANNEL=restricted-channel \
// RUN:   %target-swift-frontend -typecheck %t/ResilientClient.swift \
// RUN:   -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -verify -verify-ignore-unknown -verify-additional-prefix reject-

/// Building a ResilientLib client should succeed for a channel compiler with SWIFT_IGNORE_SWIFTMODULE_REVISION
// RUN: env SWIFT_FORCE_SWIFTMODULE_CHANNEL=restricted-channel SWIFT_IGNORE_SWIFTMODULE_REVISION=true \
// RUN:   %target-swift-frontend -typecheck %t/ResilientClient.swift \
// RUN:   -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache


/// 4. Test importing the channel restricted library
//--- ChannelClient.swift
import ChannelLib // expected-reject-error {{the binary module for 'ChannelLib' was compiled for 'restricted-channel', it cannot be imported by the current compiler which is aligned with 'other-channel'. Binary module loaded from path:}}
foo()

/// Importing ChannelLib should succeed with the same channel.
// RUN: env SWIFT_FORCE_SWIFTMODULE_CHANNEL=restricted-channel \
// RUN:   %target-swift-frontend -typecheck %t/ChannelClient.swift \
// RUN:   -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache

/// Importing ChannelLib should succeed with a dev compiler.
// RUN: %target-swift-frontend -typecheck %t/ChannelClient.swift \
// RUN:   -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache

/// Importing ChannelLib from a different channel should be rejected.
// RUN: env SWIFT_FORCE_SWIFTMODULE_CHANNEL=other-channel \
// RUN:   %target-swift-frontend -typecheck %t/ChannelClient.swift \
// RUN:   -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -verify -verify-ignore-unknown -verify-additional-prefix reject-
