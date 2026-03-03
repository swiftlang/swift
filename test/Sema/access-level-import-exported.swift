/// Test that an @_exported import is preferred to local non-public imports.
// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/TargetLib.swift -o %t
// RUN: %target-swift-frontend -emit-module %t/IndirectLib.swift -I %t -o %t

/// Check acceptable client configurations to access TargetLib publicly.
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   %t/Client_ImportDirect.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   %t/Client_ImportDirect.swift %t/Client_FileReexport.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   %t/Client_ImportDirectAsPrivate.swift %t/Client_FileReexport.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   %t/Client_ImportIndirectModule.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   %t/Client_ImportIndirectModule.swift %t/Client_FileReexport.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   %t/Client_ImportIndirectModuleAsPrivate.swift %t/Client_FileReexport.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t \
// RUN:   %t/Client_ImportIndirectLocal.swift %t/Client_FileReexport.swift

//--- TargetLib.swift
public struct TargetType {
    public init() {}
}

//--- IndirectLib.swift
@_exported import TargetLib

//--- Client_FileReexport.swift
@_exported public import TargetLib

//--- Client_ImportDirect.swift
public import TargetLib
public func user(t: TargetType) {}

//--- Client_ImportDirectAsPrivate.swift
fileprivate import TargetLib
public func user(t: TargetType) {}
// Unrestricted as it's @_exported elsewhere in the module

//--- Client_ImportIndirectModule.swift
public import IndirectLib
public func user(t: TargetType) {}

//--- Client_ImportIndirectModuleAsPrivate.swift
fileprivate import IndirectLib
public func user(t: TargetType) {}
// Unrestricted as it's @_exported elsewhere in the module

//--- Client_ImportIndirectLocal.swift
public func user(t: TargetType) {}
