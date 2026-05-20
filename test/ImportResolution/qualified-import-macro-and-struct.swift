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

// The struct is accessible.
import struct StructAndMacro.Foo // no-error
func testStruct() {
  _ = Foo()
}

//--- ClientMacro.swift

// Scoped imports do not bring macros into scope.
import struct StructAndMacro.Foo
struct Bar {
  @Foo var x: Int // expected-error {{struct 'Foo' cannot be used as an attribute}}
}

//--- ClientUnscoped.swift

// With an unscoped import, both the struct and the macro are accessible.
import StructAndMacro
func testStruct() {
  _ = Foo()
}
struct Baz {
  @Foo var x: Int // expected-error {{external macro implementation type 'NonExistent.NonExistent' could not be found for macro 'Foo()'}}
}

// Module-qualified attribute also resolves to the macro.
struct BazQualified {
  @StructAndMacro.Foo var x: Int // expected-error {{external macro implementation type 'NonExistent.NonExistent' could not be found for macro 'Foo()'}}
}

//--- ClientBoth.swift

// When both scoped and unscoped imports are present, the macro is still
// accessible through the unscoped import.
import StructAndMacro
import struct StructAndMacro.Foo
func testStruct() {
  _ = Foo()
}
struct Qux {
  @Foo var x: Int // expected-error {{external macro implementation type 'NonExistent.NonExistent' could not be found for macro 'Foo()'}}
}
