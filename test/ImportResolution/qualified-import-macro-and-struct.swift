// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -o %t/StructAndMacro.swiftmodule %t/StructAndMacro.swift -module-name StructAndMacro
// RUN: %target-swift-frontend -typecheck -I %t %t/ClientStruct.swift -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck -I %t %t/ClientMacro.swift -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck -I %t %t/ClientUnscoped.swift -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck -I %t %t/ClientBoth.swift -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -emit-module -o %t/ClientStruct.swiftmodule -I %t %t/ClientStruct.swift -module-name ClientStruct
// RUN: %target-swift-ide-test -print-module -module-to-print ClientStruct -source-filename %t/ClientStruct.swift -I %t

//--- StructAndMacro.swift

public struct Foo {
  public init() {}
}

@attached(accessor)
public macro Foo() = #externalMacro(module: "NonExistent", type: "NonExistent")

//--- ClientStruct.swift

// Struct is accessible.
import struct StructAndMacro.Foo // no-error
func testStruct() {
  _ = Foo()
}

//--- ClientMacro.swift

// Scoped import does not expose the macro.
import struct StructAndMacro.Foo
struct Bar {
  @Foo var x: Int // expected-error {{struct 'Foo' cannot be used as an attribute}}
}

// Module-qualified attribute also does not.
struct BarQualified {
  @StructAndMacro.Foo var x: Int // expected-error {{struct 'Foo' cannot be used as an attribute}}
}

//--- ClientUnscoped.swift

// Unscoped import exposes both struct and macro.
import StructAndMacro
func testStruct() {
  _ = Foo()
}
struct Baz {
  @Foo var x: Int // expected-error {{external macro implementation type 'NonExistent.NonExistent' could not be found for macro 'Foo()'}}
}

// Module-qualified attribute resolves to the macro.
struct BazQualified {
  @StructAndMacro.Foo var x: Int // expected-error {{external macro implementation type 'NonExistent.NonExistent' could not be found for macro 'Foo()'}}
}

//--- ClientBoth.swift

// Unscoped import still exposes the macro alongside the scoped struct.
import StructAndMacro
import struct StructAndMacro.Foo
func testStruct() {
  _ = Foo()
}
struct Qux {
  @Foo var x: Int // expected-error {{external macro implementation type 'NonExistent.NonExistent' could not be found for macro 'Foo()'}}
}
