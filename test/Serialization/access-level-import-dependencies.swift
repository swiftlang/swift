/// Check that non-public dependencies are hidden from clients.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Prepare a module to hide or show depending on the import access-level.
// RUN: %target-swift-frontend -emit-module %t/HiddenDep.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution

//--- HiddenDep.swift

//--- PublicDep.swift
public import HiddenDep

//--- PackageDep.swift
package import HiddenDep

//--- InternalDep.swift
internal import HiddenDep

//--- FileprivateDep.swift
fileprivate import HiddenDep

//--- PrivateDep.swift
private import HiddenDep

/// With resilience, non-public dependencies should be hidden.
// RUN: %target-swift-frontend -emit-module %t/PublicDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PackageDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -package-name MyPackage
// RUN: %target-swift-frontend -emit-module %t/InternalDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/FileprivateDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PrivateDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution

// RUN: %target-swift-frontend -typecheck %t/ClientOfPublic.swift -I %t \
// RUN:   -package-name MyOtherPackage \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s
// VISIBLE-DEP: loaded module 'HiddenDep'
//--- ClientOfPublic.swift
import PublicDep

// RUN: %target-swift-frontend -typecheck %t/ClientOfNonPublic.swift -I %t \
// RUN:   -package-name pkg \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=HIDDEN-DEP %s
// HIDDEN-DEP-NOT: loaded module 'HiddenDep'
//--- ClientOfNonPublic.swift
import PackageDep
import InternalDep
import FileprivateDep
import PrivateDep

/// Without resilience, all access-level dependencies are visible to clients.
// RUN: %target-swift-frontend -emit-module %t/PublicDep.swift -o %t -I %t
// RUN: %target-swift-frontend -emit-module %t/PackageDep.swift -o %t -I %t \
// RUN:   -package-name MyPackage
// RUN: %target-swift-frontend -emit-module %t/InternalDep.swift -o %t -I %t
// RUN: %target-swift-frontend -emit-module %t/FileprivateDep.swift -o %t -I %t
// RUN: %target-swift-frontend -emit-module %t/PrivateDep.swift -o %t -I %t

// RUN: %target-swift-frontend -typecheck %t/ClientOfPublic.swift -I %t \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s
// RUN: %target-swift-frontend -typecheck %t/ClientOfNonPublic.swift -I %t \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s

/// Even with resilience and testing enabled, all non-public dependencies are
/// hidden if there are no testable imports.
// RUN: %target-swift-frontend -emit-module %t/PublicDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing
// RUN: %target-swift-frontend -emit-module %t/PackageDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing -package-name MyPackage
// RUN: %target-swift-frontend -emit-module %t/InternalDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing
// RUN: %target-swift-frontend -emit-module %t/FileprivateDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing
// RUN: %target-swift-frontend -emit-module %t/PrivateDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing

// RUN: %target-swift-frontend -typecheck %t/ClientOfPublic.swift -I %t \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s
// RUN: %target-swift-frontend -typecheck %t/ClientOfNonPublic.swift -I %t \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=HIDDEN-DEP %s

/// With testable imports, transitive dependencies are required.
//--- TestableClientOfPublic.swift
@testable import PublicDep

//--- TestableClientOfNonPublic.swift
@testable import PackageDep // expected-error {{missing required module 'HiddenDep'}}
@testable import InternalDep // expected-error {{missing required module 'HiddenDep'}}
@testable import FileprivateDep // expected-error {{missing required module 'HiddenDep'}}
@testable import PrivateDep // expected-error {{missing required module 'HiddenDep'}}

// RUN: %target-swift-frontend -typecheck %t/TestableClientOfPublic.swift -I %t \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s
// RUN: %target-swift-frontend -typecheck %t/TestableClientOfNonPublic.swift -I %t \
// RUN:   -package-name pkg \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s

/// In the case of a testable of a module reexporting another Swift module,
/// only non-public transitive dependencies from the first module are required.
/// Non-public imports from the reexported modules are not loaded, we could
/// revisit this if desired.
// RUN: %target-swift-frontend -emit-module %t/Exporter.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing
// RUN: %target-swift-frontend -typecheck %t/ExporterClient.swift -I %t \
// RUN:   -index-system-modules -index-ignore-stdlib -index-store-path %t/idx \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefixes=CHECK-EXPORTER,HIDDEN-DEP %s
// CHECK-EXPORTER: 'InternalDep' has an ignored transitive dependency on 'HiddenDep'

//--- Exporter.swift
@_exported import InternalDep

//--- ExporterClient.swift
@testable import Exporter

/// Fail if the transitive dependency is missing.
// RUN: rm %t/HiddenDep.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/TestableClientOfNonPublic.swift -I %t \
// RUN:   -package-name pkg \
// RUN:   -verify -show-diagnostics-after-fatal

/// In a multi-file scenario, we try and fail to load the transitive dependency
/// only for @testable imports, not regular imports.
// RUN: %target-swift-frontend -typecheck -I %t \
// RUN:   %t/TestableClientOfNonPublic_FileA.swift \
// RUN:   %t/TestableClientOfNonPublic_FileB.swift \
// RUN:   -verify -show-diagnostics-after-fatal
// RUN: %target-swift-frontend -typecheck -wmo -I %t \
// RUN:   %t/TestableClientOfNonPublic_FileA.swift \
// RUN:   %t/TestableClientOfNonPublic_FileB.swift \
// RUN:   -verify -show-diagnostics-after-fatal

//--- TestableClientOfNonPublic_FileA.swift
import InternalDep
@testable import FileprivateDep // expected-error {{missing required module 'HiddenDep'}}

//--- TestableClientOfNonPublic_FileB.swift
@testable import InternalDep // expected-error {{missing required module 'HiddenDep'}}
import FileprivateDep
