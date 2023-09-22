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
// RUN: %target-swift-frontend -typecheck %t/ClientWithoutTheFlag.swift -I %t -verify \
// RUN:   -package-name package
// RUN: %target-swift-frontend -typecheck %t/ClientWithoutTheFlag.swift -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport \
// RUN:   -package-name package
// REQUIRES: asserts

/// swiftinterfaces don't need the flag.
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t

//--- PublicLib.swift
//--- PackageLib.swift
//--- InternalLib.swift
//--- FileprivateLib.swift
//--- PrivateLib.swift

//--- ClientWithoutTheFlag.swift
public import PublicLib // expected-error@:1 {{Access level on imports require '-enable-experimental-feature AccessLevelOnImport'}}
// expected-warning @-1 {{public import of 'PublicLib' was not used in public declarations or inlinable code}}
package import PackageLib // expected-error@:1 {{Access level on imports require '-enable-experimental-feature AccessLevelOnImport'}}
// expected-warning @-1 {{package import of 'PackageLib' was not used in package declarations}}
internal import InternalLib // expected-error@:1 {{Access level on imports require '-enable-experimental-feature AccessLevelOnImport'}}
fileprivate import FileprivateLib // expected-error@:1 {{Access level on imports require '-enable-experimental-feature AccessLevelOnImport'}}
private import PrivateLib // expected-error@:1 {{Access level on imports require '-enable-experimental-feature AccessLevelOnImport'}}

//--- Client.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -enable-library-evolution -package-name MyPackage
public import PublicLib
package import PackageLib
internal import InternalLib
fileprivate import FileprivateLib
private import PrivateLib
