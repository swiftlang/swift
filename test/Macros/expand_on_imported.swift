// REQUIRES: swift_swift_parser, executable_test, concurrency

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// Build the macro library to give us access to AddAsync.
// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/macro_library.swiftmodule %S/Inputs/macro_library.swift -module-name macro_library -load-plugin-library %t/%target-library-name(MacroDefinition)

// Diagnostics testing
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -swift-version 5 -enable-experimental-feature MacrosOnImports -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name ModuleUser %s -I %t

import CompletionHandlerGlobals
import macro_library

// Make sure that @AddAsync works at all.
@AddAsync
@available(SwiftStdlib 5.1, *)
func asyncTest(_ value: Int, completionHandler: @escaping (String) -> Void) {
  completionHandler(String(value))
}

@available(SwiftStdlib 5.1, *)
func testAll(x: Double, y: Double, computer: SlowComputer) async {
  _ = await asyncTest(17)

  let _: Double = await async_divide(1.0, 2.0)
  let _: Double = await computer.divide(x, y)
}
