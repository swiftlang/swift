// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/indirects.swiftmodule %S/Inputs/implementation-only-imports/indirects.swift
// RUN: %target-swift-frontend -emit-module -o %t/directs.swiftmodule -I %t %S/Inputs/implementation-only-imports/directs.swift

// RUN: %target-swift-frontend -typecheck -verify %s -I %t

@_implementationOnly import directs

// Types

@inlinable
public func testStructFromIndirect() {
  _ = StructFromIndirect() // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testAliasFromIndirect() {
  _ = AliasFromIndirect() // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testGenericAliasFromIndirect() {
  _ = GenericAliasFromIndirect<Int>() // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

// Functions

@inlinable
public func testFunctionFromIndirect() {
  globalFunctionFromIndirect() // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

// Variables

@inlinable
public func testVariableFromIndirect_get() {
  _ = globalVariableFromIndirect // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testVariableFromIndirect_set() {
  globalVariableFromIndirect = 5 // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}
