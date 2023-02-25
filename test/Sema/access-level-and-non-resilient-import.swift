// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/DefaultLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/InternalLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/FileprivateLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift -o %t

/// A resilient client will error on public imports.
// RUN: %target-swift-frontend -typecheck %t/Client_Swift5.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-experimental-feature AccessLevelOnImport -verify
// RUN: %target-swift-frontend -typecheck %t/Client_Swift6.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -enable-experimental-feature AccessLevelOnImport -verify

/// A non-resilient client doesn't complain.
// RUN: %target-swift-frontend -typecheck %t/Client_Swift5.swift -I %t \
// RUN:   -swift-version 5 \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// RUN: %target-swift-frontend -typecheck %t/Client_Swift6.swift -I %t \
// RUN:   -swift-version 6 \
// RUN:   -enable-experimental-feature AccessLevelOnImport

//--- DefaultLib.swift
//--- PublicLib.swift
//--- PackageLib.swift
//--- InternalLib.swift
//--- FileprivateLib.swift
//--- PrivateLib.swift

//--- Client_Swift5.swift

import DefaultLib // expected-error {{module 'DefaultLib' was not compiled with library evolution support; using it means binary compatibility for 'Client_Swift5' can't be guaranteed}} {{1-1=internal }}
public import PublicLib // expected-error {{module 'PublicLib' was not compiled with library evolution support; using it means binary compatibility for 'Client_Swift5' can't be guaranteed}} {{1-7=internal}}
package import PackageLib
internal import InternalLib
fileprivate import FileprivateLib
private import PrivateLib

//--- Client_Swift6.swift

import DefaultLib
public import PublicLib // expected-error {{module 'PublicLib' was not compiled with library evolution support; using it means binary compatibility for 'Client_Swift6' can't be guaranteed}} {{1-7=internal}}
package import PackageLib
internal import InternalLib
fileprivate import FileprivateLib
private import PrivateLib
