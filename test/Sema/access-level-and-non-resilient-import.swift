/// Check diagnostics on a resilient modules importing publicly a
/// a non-resilient module.

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/DefaultLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/InternalLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/FileprivateLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift -o %t

/// A resilient client will error on public imports.
/// Keep AccessLevelOnImport to get the error from Swift 6 in Swift 5.
// RUN: %target-swift-frontend -typecheck %t/Client_Swift5.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-experimental-feature AccessLevelOnImport -verify \
// RUN:   -package-name pkg
// RUN: %target-swift-frontend -typecheck %t/Client_Swift6.swift -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -verify -package-name pkg

/// A non-resilient client doesn't complain.
// RUN: %target-swift-frontend -typecheck %t/Client_Swift5.swift -I %t \
// RUN:   -swift-version 5 \
// RUN:   -enable-experimental-feature AccessLevelOnImport \
// RUN:   -package-name pkg
// RUN: %target-swift-frontend -typecheck %t/Client_Swift6.swift -I %t \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -package-name pkg

// REQUIRES: swift_feature_AccessLevelOnImport
// REQUIRES: swift_feature_InternalImportsByDefault

//--- DefaultLib.swift
//--- PublicLib.swift
//--- PackageLib.swift
//--- InternalLib.swift
//--- FileprivateLib.swift
//--- PrivateLib.swift

//--- Client_Swift5.swift

import DefaultLib // expected-error {{module 'DefaultLib' was not compiled with library evolution support; using it means binary compatibility for 'Client_Swift5' can't be guaranteed}} {{1-1=internal }}
public import PublicLib // expected-error {{module 'PublicLib' was not compiled with library evolution support; using it means binary compatibility for 'Client_Swift5' can't be guaranteed}} {{1-7=internal}}
// expected-warning @-1 {{public import of 'PublicLib' was not used in public declarations or inlinable code}}
package import PackageLib
// expected-warning @-1 {{package import of 'PackageLib' was not used in package declarations}}
internal import InternalLib
fileprivate import FileprivateLib
private import PrivateLib

//--- Client_Swift6.swift

import DefaultLib
public import PublicLib // expected-error {{module 'PublicLib' was not compiled with library evolution support; using it means binary compatibility for 'Client_Swift6' can't be guaranteed}} {{1-8=}}
// expected-warning @-1 {{public import of 'PublicLib' was not used in public declarations or inlinable code}}
package import PackageLib
// expected-warning @-1 {{package import of 'PackageLib' was not used in package declarations}}
internal import InternalLib
fileprivate import FileprivateLib
private import PrivateLib
