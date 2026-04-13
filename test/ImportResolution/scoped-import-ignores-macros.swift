// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/StructAndMacro.swiftmodule %S/Inputs/StructAndMacro.swift -module-name StructAndMacro
// RUN: %target-swift-frontend -typecheck -I %t %s -verify

// Scoped import of a struct should not be ambiguous when a macro with the
// same name exists in the module.
import struct StructAndMacro.Foo // no-error

func test() {
  _ = Foo(value: 42)
}
