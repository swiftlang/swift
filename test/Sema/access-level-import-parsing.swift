// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/InternalLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/FileprivateLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/OpenLib.swift -o %t

/// Check that all access levels are accepted, except for 'open'.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name package -verify

//--- PublicLib.swift
//--- PackageLib.swift
//--- InternalLib.swift
//--- FileprivateLib.swift
//--- PrivateLib.swift
//--- OpenLib.swift

//--- Client.swift
public import PublicLib // expected-warning {{public import of 'PublicLib' was not used in public declarations or inlinable code}}
package import PackageLib // expected-warning {{package import of 'PackageLib' was not used in package declarations}}
internal import InternalLib
fileprivate import FileprivateLib
private import PrivateLib
open import OpenLib // expected-error {{The access level 'open' is unsupported on imports: only 'public', 'package', 'internal', 'fileprivate' and 'private' are accepted}}
