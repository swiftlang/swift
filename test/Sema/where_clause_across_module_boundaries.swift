// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/ModuleA.swiftmodule %S/Inputs/where_clause_across_module_boundaries_module.swift
// RUN: %target-typecheck-verify-swift -I %t

// SR-15807:
// Associated Type Inference fails across module boundaries
// Self bounds from where clause cannot be accessed across modules.
// This test is intended to test whether it can use generic signature to get self bounds.
import ModuleA

struct ModuleBFoo: Codable, DefaultsSerializable {
}

enum ModuleBBar: Int, Codable, DefaultsSerializable { // expected-error {{type 'ModuleBBar' does not conform to protocol 'DefaultsSerializable'}}
  case foo, bar
}
