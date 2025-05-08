// REQUIRES: swift_swift_parser, executable_test
// REQUIRES: swift_feature_MacrosOnImports

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// Build the macro library to give us access to ExpandTypeError.
// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/macro_library.swiftmodule %S/Inputs/macro_library.swift -module-name macro_library -load-plugin-library %t/%target-library-name(MacroDefinition)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -swift-version 5 -enable-experimental-feature MacrosOnImports -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name ErrorModuleUser %s -I %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -verify -swift-version 5 -enable-experimental-feature MacrosOnImports -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name ErrorModuleUser %s -I %t

import ImportedMacroError
import macro_library

foo(42)

// CHECK: func async_divide(_ x: Double, _ y: Double, _ completionHandler: @escaping (Double) -> Void)

// CHECK: extension SlowComputer
// CHECK: public func divide(_ x: Double, _ y: Double) async -> Double

// CHECK: func async_divide(_ x: Double, _ y: Double) async -> Double


// DIAGS-NOT: error

