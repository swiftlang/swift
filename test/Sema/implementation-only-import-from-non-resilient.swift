// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Build 2 libs.
// RUN: %target-swift-frontend -emit-module %t/empty.swift -o %t/SwiftModuleA.swiftmodule \
// RUN:   -enable-library-evolution -swift-version 5
// RUN: %target-swift-frontend -emit-module %t/empty.swift -o %t/SwiftModuleB.swiftmodule \
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

//--- module.modulemap
module ClangModuleA {
    header "ClangModuleA.h"

    module Submodule {
        header "ClangSubmodule.h"
    }
}

module ClangModuleB {
    header "ClangModuleB.h"
}

//--- ClangModuleA.h
//--- ClangSubmodule.h
//--- ClangModuleB.h

//--- empty.swift

//--- client-non-resilient.swift
@_implementationOnly import SwiftModuleA // expected-warning {{using '@_implementationOnly' without enabling library evolution for 'main' may lead to instability during execution}}
@_implementationOnly import SwiftModuleA // expected-warning {{using '@_implementationOnly' without enabling library evolution for 'main' may lead to instability during execution}}
import SwiftModuleB

@_implementationOnly import ClangModuleA // expected-warning {{using '@_implementationOnly' without enabling library evolution for 'main' may lead to instability during execution}}
@_implementationOnly import ClangModuleA.Submodule // expected-warning {{using '@_implementationOnly' without enabling library evolution for 'main' may lead to instability during execution}}
import ClangModuleB

//--- client-resilient.swift
@_implementationOnly import SwiftModuleA
// expected-warning @-1 {{'@_implementationOnly' is deprecated, use 'internal import' instead}}
import SwiftModuleB

@_implementationOnly import ClangModuleA
@_implementationOnly import ClangModuleA.Submodule
import ClangModuleB

//--- Crypto.swift
@_implementationOnly import SwiftModuleA // expected-warning {{using '@_implementationOnly' without enabling library evolution for 'Crypto' may lead to instability during execution}}
import SwiftModuleB
@_implementationOnly import CCryptoBoringSSL
import ClangModuleB
