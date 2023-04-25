// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/InternalLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/FileprivateLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift -o %t

/// Check flag requirement, without and with the flag.
// RUN: %target-swift-frontend -typecheck %t/ClientWithoutTheFlag.swift -I %t -verify
// RUN: %target-swift-frontend -typecheck %t/ClientWithoutTheFlag.swift -I %t \
// RUN:   -enable-experimental-feature AccessLevelOnImport
// REQUIRES: asserts

//--- PublicLib.swift
//--- PackageLib.swift
//--- InternalLib.swift
//--- FileprivateLib.swift
//--- PrivateLib.swift

//--- ClientWithoutTheFlag.swift
public import PublicLib // expected-error@:1 {{Access level on imports require '-enable-experimental-feature AccessLevelOnImport'}}
package import PackageLib // expected-error@:1 {{Access level on imports require '-enable-experimental-feature AccessLevelOnImport'}}
internal import InternalLib // expected-error@:1 {{Access level on imports require '-enable-experimental-feature AccessLevelOnImport'}}
fileprivate import FileprivateLib // expected-error@:1 {{Access level on imports require '-enable-experimental-feature AccessLevelOnImport'}}
private import PrivateLib // expected-error@:1 {{Access level on imports require '-enable-experimental-feature AccessLevelOnImport'}}
