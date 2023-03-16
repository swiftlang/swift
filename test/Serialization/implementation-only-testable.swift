/// Test the attempt to load transitive implementation-only dependencies of
/// of middle modules with testing enabled.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build the hidden implementation-only dependency and the testable module.
// RUN: %target-swift-frontend -emit-module %t/HiddenDep.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution

// RUN: %target-swift-frontend -emit-module %t/TestingEnabledLib.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution -I %t \
// RUN:   -enable-testing

/// A client should load transitive implementation-only dependencies of
/// testable dependencies when possible.
// RUN: %target-swift-frontend -emit-module %t/Client.swift -o %t \
// RUN:   -swift-version 5 -I %t -Rmodule-loading 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-module %t/ClientWithCall.swift -o %t \
// RUN:   -swift-version 5 -I %t -Rmodule-loading 2>&1 | %FileCheck %s

/// And not fail *at loading* if the dependency is missing. It may still fail
/// on decl missing because of references to the not-loaded module.
// RUN: rm %t/HiddenDep.swiftmodule

/// The transitive dependency is not loaded but a client can still build fine.
// RUN: %target-swift-frontend -emit-module %t/Client.swift -o %t \
// RUN:   -swift-version 5 -I %t -Rmodule-loading 2>&1 | \
// RUN:   %FileCheck %s --check-prefixes=CHECK-NOT-LOADED

/// Clients referencing a decl that depends on the hidden module don't see the
/// decl, it is dropped by deserialization recovery.
// RUN: %target-swift-frontend -emit-module %t/ClientWithCall.swift -o %t \
// RUN:   -swift-version 5 -I %t -verify

//--- HiddenDep.swift

public struct HiddenType {}

//--- TestingEnabledLib.swift
@_implementationOnly import HiddenDep

internal func dependsOnHiddenType() -> HiddenType { fatalError() }

//--- Client.swift

/// Note that the import doesn't have to be testable, only the imported module
/// needs to enable testing. We may want to improve upon this in the future.
import TestingEnabledLib
// CHECK: remark: loaded module 'HiddenDep'
// CHECK-NOT-LOADED-NOT: remark: loaded module 'HiddenDep'

//--- ClientWithCall.swift

@testable import TestingEnabledLib

let _ = dependsOnHiddenType() // expected-error {{cannot find 'dependsOnHiddenType' in scope}}
