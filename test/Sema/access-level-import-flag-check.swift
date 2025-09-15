// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/InternalLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/FileprivateLib.swift -o %t \
// RUN:   -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift -o %t \
// RUN:   -enable-library-evolution

/// Check flag requirement, without and with the flag.
// RUN: %target-swift-frontend -typecheck %t/ClientWithoutTheFlag.swift -I %t \
// RUN:   -package-name package -verify
// RUN: %target-swift-frontend -typecheck %t/ClientWithoutTheFlag.swift -I %t \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -package-name package -verify

/// swiftinterfaces don't need the flag.
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t

// REQUIRES: swift_feature_InternalImportsByDefault

//--- PublicLib.swift
//--- PackageLib.swift
//--- InternalLib.swift
//--- FileprivateLib.swift
//--- PrivateLib.swift

//--- ClientWithoutTheFlag.swift
public import PublicLib
// expected-warning @-1 {{public import of 'PublicLib' was not used in public declarations or inlinable code}}
package import PackageLib
// expected-warning @-1 {{package import of 'PackageLib' was not used in package declarations}}
internal import InternalLib
fileprivate import FileprivateLib
private import PrivateLib

//--- Client.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -enable-library-evolution -package-name MyPackage
public import PublicLib
package import PackageLib
internal import InternalLib
fileprivate import FileprivateLib
private import PrivateLib
