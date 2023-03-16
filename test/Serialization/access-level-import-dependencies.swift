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
// RUN:   -enable-library-evolution \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/PackageDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -package-name MyPackage \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/InternalDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/FileprivateDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/PrivateDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -enable-experimental-feature AccessLevelOnImport

// RUN: %target-swift-frontend -typecheck %t/ClientOfPublic.swift -I %t \
// RUN:   -package-name MyOtherPackage \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s
// VISIBLE-DEP: loaded module 'HiddenDep'
//--- ClientOfPublic.swift
import PublicDep

// RUN: %target-swift-frontend -typecheck %t/ClientOfNonPublic.swift -I %t \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=HIDDEN-DEP %s
// HIDDEN-DEP-NOT: loaded module 'HiddenDep'
//--- ClientOfNonPublic.swift
import PackageDep
import InternalDep
import FileprivateDep
import PrivateDep

/// Without resilience, all access-level dependencies are visible to clients.
// RUN: %target-swift-frontend -emit-module %t/PublicDep.swift -o %t -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/PackageDep.swift -o %t -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/InternalDep.swift -o %t -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/FileprivateDep.swift -o %t -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/PrivateDep.swift -o %t -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport

// RUN: %target-swift-frontend -typecheck %t/ClientOfPublic.swift -I %t \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s
// RUN: %target-swift-frontend -typecheck %t/ClientOfNonPublic.swift -I %t \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s

/// Even with resilience, all access-level dependencies are visible to clients
/// for modules with testing enabled.
// RUN: %target-swift-frontend -emit-module %t/PublicDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/PackageDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing -package-name MyPackage \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/InternalDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/FileprivateDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -emit-module %t/PrivateDep.swift -o %t -I %t \
// RUN:   -enable-library-evolution -enable-testing \
// RUN:   -enable-experimental-feature AccessLevelOnImport

// RUN: %target-swift-frontend -typecheck %t/ClientOfPublic.swift -I %t \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s
// RUN: %target-swift-frontend -typecheck %t/ClientOfNonPublic.swift -I %t \
// RUN:   -Rmodule-loading 2>&1 | %FileCheck -check-prefix=VISIBLE-DEP %s
