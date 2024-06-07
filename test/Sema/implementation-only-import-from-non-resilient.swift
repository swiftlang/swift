// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build 2 libs.
// RUN: %target-swift-frontend -emit-module %t/empty.swift -o %t/A.swiftmodule \
// RUN:   -enable-library-evolution -swift-version 5
// RUN: %target-swift-frontend -emit-module %t/empty.swift -o %t/B.swiftmodule \
// RUN:   -enable-library-evolution -swift-version 5

/// Build a client with and without library-evolution.
// RUN: %target-swift-frontend -typecheck %t/client-non-resilient.swift -I %t -verify
// RUN: %target-swift-frontend -typecheck %t/client-resilient.swift -I %t -verify \
// RUN:   -enable-library-evolution -swift-version 5

/// Some imports are exempt.
// RUN: %target-swift-frontend -emit-module %t/empty.swift \
// RUN:   -o %t/CCryptoBoringSSL.swiftmodule \
// RUN:   -enable-library-evolution -swift-version 5
// RUN: %target-swift-frontend -typecheck %t/Crypto.swift -I %t -verify \
// RUN:   -module-name Crypto

//--- empty.swift

//--- client-non-resilient.swift
@_implementationOnly import A // expected-warning {{using '@_implementationOnly' without enabling library evolution for 'main' may lead to instability during execution}}
import B

//--- client-resilient.swift
@_implementationOnly import A
// expected-warning @-1 {{'@_implementationOnly' is deprecated, use 'internal import' instead}}
import B

//--- Crypto.swift
@_implementationOnly import A // expected-warning {{using '@_implementationOnly' without enabling library evolution for 'Crypto' may lead to instability during execution}}
import B
@_implementationOnly import CCryptoBoringSSL
