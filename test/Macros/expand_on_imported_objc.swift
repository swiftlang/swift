// REQUIRES: swift_swift_parser, executable_test, objc_interop, concurrency
// REQUIRES: swift_feature_MacrosOnImports
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// Build the macro library to give us access to AddAsync.
// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/macro_library.swiftmodule %S/Inputs/macro_library.swift -module-name macro_library -load-plugin-library %t/%target-library-name(MacroDefinition)

// Diagnostics testing
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -swift-version 5 -enable-experimental-feature MacrosOnImports -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name ModuleUser %s -I %t -DTEST_DIAGNOSTICS

// Emit IR just to make sure nothing else fails.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -swift-version 5 -g -enable-experimental-feature MacrosOnImports -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name ModuleUser %s -I %t | %FileCheck %s

import CompletionHandlerGlobals
import macro_library

@available(SwiftStdlib 5.1, *)
func testAll(x: Double, y: Double, computer: Computer, untypedComputer: AnyObject) async {
  let _: Double = await computer.multiply(x, by: y)

  #if TEST_DIAGNOSTICS
  // expected-error@+1{{missing argument for parameter 'afterDone' in call}}
  untypedComputer.multiply(x, by: y)
  #endif
}

// CHECK: define linkonce_odr hidden swifttailcc void @"$sSo8ComputerC8multiply_2byS2d_SdtYaF"
