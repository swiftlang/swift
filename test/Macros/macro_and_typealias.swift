// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/variadic_macros.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -disable-availability-checking -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5

@freestanding(expression) public macro Print<each Value>(_ value: repeat each Value) = #externalMacro(module: "MacroDefinition", type: "PrintMacro")
@freestanding(expression) public macro OtherPrint<each Value>(_ value: repeat each Value) = #externalMacro(module: "MacroDefinition", type: "PrintMacro")

public struct Printer<Value> {
  init(_: (Value) -> Void) {}
}

typealias Print = Printer
typealias OtherPrint<T> = Printer<T>

struct Test {
  struct Object {
    var prop: Int
  }

  func test() {
    let _ = Print<Object> { // Ok
      compute(root: $0, \.prop)
    }

    let _ = OtherPrint<Object> { // Ok
      compute(root: $0, \.prop)
    }
  }

  func compute<R, V>(root: R, _: KeyPath<R, V>) {}
}
