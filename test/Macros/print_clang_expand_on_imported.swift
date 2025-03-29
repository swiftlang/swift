// REQUIRES: swift_swift_parser, executable_test, concurrency
// REQUIRES: swift_feature_MacrosOnImports

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// Build the macro library to give us access to AddAsync.
// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/macro_library.swiftmodule %S/Inputs/macro_library.swift -module-name macro_library -load-plugin-library %t/%target-library-name(MacroDefinition)

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -typecheck -print-module -print-implicit-attrs -source-filename %s -module-to-print=CompletionHandlerGlobals -I %t -function-definitions=false -load-plugin-library %t/%target-library-name(MacroDefinition) -import-module macro_library -enable-experimental-feature MacrosOnImports > %t/imported.printed.txt 2> %t/stderr.log
// RUN: %FileCheck -input-file %t/imported.printed.txt %s
// RUN: echo "" >> %t/stderr.log
// RUN: %FileCheck -check-prefix DIAGS -input-file %t/stderr.log %s

import CompletionHandlerGlobals

// CHECK: func async_divide(_ x: Double, _ y: Double, _ completionHandler: @escaping (Double) -> Void)

// CHECK: extension SlowComputer
// CHECK: public func divide(_ x: Double, _ y: Double) async -> Double

// CHECK: func async_divide(_ x: Double, _ y: Double) async -> Double


// DIAGS-NOT: error
