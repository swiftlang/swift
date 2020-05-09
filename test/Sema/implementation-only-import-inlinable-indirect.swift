// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/indirects.swiftmodule %S/Inputs/implementation-only-imports/indirects.swift
// RUN: %target-swift-frontend -emit-module -o %t/directs.swiftmodule -I %t %S/Inputs/implementation-only-imports/directs.swift

// RUN: %target-swift-frontend -typecheck -verify %s -I %t

@_implementationOnly import directs

// Types

@inlinable
public func testStructFromIndirect() {
  _ = StructFromIndirect() // expected-error {{struct 'StructFromIndirect' cannot be used in an '@inlinable' function because 'indirects' was imported implementation-only}}
}

@inlinable
public func testAliasFromIndirect() {
  _ = AliasFromIndirect() // expected-error {{type alias 'AliasFromIndirect' cannot be used in an '@inlinable' function because 'indirects' was imported implementation-only}}
}

@inlinable
public func testGenericAliasFromIndirect() {
  _ = GenericAliasFromIndirect<Int>() // expected-error {{type alias 'GenericAliasFromIndirect' cannot be used in an '@inlinable' function because 'indirects' was imported implementation-only}}
}

// Functions

@inlinable
public func testFunctionFromIndirect() {
  globalFunctionFromIndirect() // expected-error {{global function 'globalFunctionFromIndirect()' cannot be used in an '@inlinable' function because 'indirects' was imported implementation-only}}
}

// Variables

@inlinable
public func testVariableFromIndirect_get() {
  _ = globalVariableFromIndirect // expected-error {{var 'globalVariableFromIndirect' cannot be used in an '@inlinable' function because 'indirects' was imported implementation-only}}
}

@inlinable
public func testVariableFromIndirect_set() {
  globalVariableFromIndirect = 5 // expected-error {{var 'globalVariableFromIndirect' cannot be used in an '@inlinable' function because 'indirects' was imported implementation-only}}
}
