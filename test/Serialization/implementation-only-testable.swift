/// Implementation-only dependencies of modules imported as testable are
/// optional, we attempt to load them but don't error if they are missing.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build the hidden implementation-only dependency and the testable module.
// RUN: %target-swift-frontend -emit-module %t/HiddenDep.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/TestedLib.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution -I %t \
// RUN:   -enable-testing

/// A client should ignore the transitive implementation-only dependency for a
/// normal import.
// RUN: %target-swift-frontend -typecheck %t/ClientNormalImport.swift -o %t \
// RUN:   -swift-version 5 -I %t -Rmodule-loading 2>&1 | \
// RUN:   %FileCheck --check-prefix=CHECK-NOT-LOADED %s

/// A client should load the transitive implementation-only dependency of
/// a module imported @testable.
// RUN: %target-swift-frontend -typecheck %t/ClientTestableImport.swift -o %t \
// RUN:   -swift-version 5 -I %t -Rmodule-loading 2>&1 | %FileCheck %s

/// If the dependency is missing we don't fail at loading, but we may still
/// fail later accessing decls missing because of references to the not-loaded module.
// RUN: rm %t/HiddenDep.swiftmodule

/// The transitive dependency is not loaded but a client can still build fine.
// RUN: %target-swift-frontend -typecheck %t/ClientNormalImport.swift -o %t \
// RUN:   -swift-version 5 -I %t -Rmodule-loading 2>&1 | \
// RUN:   %FileCheck %s --check-prefix=CHECK-NOT-LOADED

/// Clients referencing a decl that depends on the hidden module don't see the
/// decl, it is dropped by deserialization recovery.
// RUN: %target-swift-frontend -typecheck %t/ClientTestableImport.swift -o %t \
// RUN:   -swift-version 5 -I %t -verify

//--- HiddenDep.swift

public struct HiddenType {}

//--- TestedLib.swift

@_implementationOnly import HiddenDep

internal func dependsOnHiddenType() -> HiddenType { fatalError() }

//--- ClientNormalImport.swift

/// Note that the import doesn't have to be testable, only the imported module
/// needs to enable testing. We may want to improve upon this in the future.
import TestedLib

// Note: extra newlines below ensure that context printing doesn't show the
// lines that we shouldn't see.


// CHECK-NOT-LOADED-NOT: remark: loaded module 'HiddenDep'

//--- ClientTestableImport.swift

@testable import TestedLib
// CHECK: remark: loaded module 'HiddenDep'

let _ = dependsOnHiddenType() // expected-error {{cannot find 'dependsOnHiddenType' in scope}}
