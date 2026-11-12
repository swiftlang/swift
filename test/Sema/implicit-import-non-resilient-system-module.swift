/// A resilient module isn't warned about the implicit import of a non-resilient
/// system module, like that of _Concurrency or _StringProcessing when the
/// standard library isn't built with library evolution.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/SystemLib.swift -o %t/system/SystemLib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/UserLib.swift -o %t/user/UserLib.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/Client.swift -verify \
// RUN:   -enable-library-evolution -Isystem %t/system -import-module SystemLib

/// An explicit import of the system module is still diagnosed.
// RUN: %target-swift-frontend -typecheck %t/ExplicitClient.swift -verify \
// RUN:   -enable-library-evolution -Isystem %t/system

/// So is the implicit import of a non-system module.
// RUN: %target-swift-frontend -typecheck %t/Client.swift \
// RUN:   -enable-library-evolution -I %t/user -import-module UserLib 2>&1 \
// RUN:   | %FileCheck %s
// CHECK: warning: module 'UserLib' was not compiled with library evolution support

//--- SystemLib.swift
public struct S {}

//--- UserLib.swift
public struct U {}

//--- Client.swift
public func f() {}

//--- ExplicitClient.swift
import SystemLib // expected-warning {{module 'SystemLib' was not compiled with library evolution support; using it means binary compatibility for 'ExplicitClient' can't be guaranteed}}
