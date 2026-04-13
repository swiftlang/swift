// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -o %t/StructAndMacro.swiftmodule %t/StructAndMacro.swift -module-name StructAndMacro
// RUN: %target-swift-frontend -typecheck -I %t %t/Client.swift -verify
// RUN: %target-swift-frontend -emit-module -o %t/Client.swiftmodule -I %t %t/Client.swift -module-name Client

//--- StructAndMacro.swift

public struct Foo {
  public init(value: Int) {}
}

@freestanding(expression)
public macro Foo() -> Int = #externalMacro(module: "NonExistent", type: "NonExistent")

//--- Client.swift

import struct StructAndMacro.Foo // no-error

func test() {
  _ = Foo(value: 42)
}
