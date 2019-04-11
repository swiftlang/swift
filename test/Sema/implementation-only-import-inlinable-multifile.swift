// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/indirects.swiftmodule %S/Inputs/implementation-only-imports/indirects.swift
// RUN: %target-swift-frontend -emit-module -o %t/directs.swiftmodule -I %t %S/Inputs/implementation-only-imports/directs.swift

// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/implementation-only-imports/secondary_file.swift -I %t

@_implementationOnly import directs
// 'indirects' is imported for re-export in a secondary file

// Types

@inlinable
public func testStructFromDirect() {
  _ = StructFromDirect() // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testStructFromIndirect() {
  _ = StructFromIndirect()
}

@inlinable
public func testAliasFromDirect() {
  _ = AliasFromDirect() // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testAliasFromIndirect() {
  _ = AliasFromIndirect()
}

@inlinable
public func testGenericAliasFromDirect() {
  _ = GenericAliasFromDirect<Int>() // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testGenericAliasFromIndirect() {
  let tmp: GenericAliasFromIndirect<Int>?
  _ = tmp
}

// Functions

@inlinable
public func testFunctionFromDirect() {
  globalFunctionFromDirect() // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testFunctionFromIndirect() {
  globalFunctionFromIndirect()
}

// Variables

@inlinable
public func testVariableFromDirect_get() {
  _ = globalVariableFromDirect // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testVariableFromIndirect_get() {
  _ = globalVariableFromIndirect
}

@inlinable
public func testVariableFromDirect_set() {
  globalVariableFromDirect = 5 // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testVariableFromIndirect_set() {
  globalVariableFromIndirect = 5
}

// Extensions

@inlinable
public func testExtensionMethod(s: inout StructFromIndirect) {
  s.extensionMethodFromDirect() // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testExtensionProperty_get(s: inout StructFromIndirect) {
  _ = s.extensionPropertyFromDirect // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testExtensionProperty_set(s: inout StructFromIndirect) {
  s.extensionPropertyFromDirect = 5 // expected-error {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testExtensionSubscript_get(s: inout StructFromIndirect) {
  // FIXME: why is this error being double-emitted?
  _ = s[extensionSubscript: 0] // expected-error 2 {{cannot be used in an inlinable function because its module was imported implementation-only}}
}

@inlinable
public func testExtensionSubscript_set(s: inout StructFromIndirect) {
  // FIXME: why is this error being double-emitted?
  s[extensionSubscript: 0] = 5 // expected-error 2 {{cannot be used in an inlinable function because its module was imported implementation-only}}
}
