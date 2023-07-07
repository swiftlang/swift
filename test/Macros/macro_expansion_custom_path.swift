// REQUIRES: swift_swift_parser

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/macro_library.swiftmodule %S/Inputs/macro_library.swift -module-name macro_library -load-plugin-library %t/%target-library-name(MacroDefinition)

// Debug info IR path remapping testing
// RUN: %target-swift-frontend -swift-version 5 -emit-ir -I%t -verify -primary-file %s -verify-ignore-unknown -load-plugin-library %t/%target-library-name(MacroDefinition) -emit-macro-expansion-files-path=%t/custom-expansions -debug-prefix-map %t/custom-expansions=REMAPPED -o - -g | %FileCheck --check-prefix CHECK-IR %s
// RUN: ls %t/custom-expansions | %FileCheck --check-prefix CHECK-FILES %s

import macro_library

// CHECK-IR: !{{[0-9]+}} = !DIFile(filename: "REMAPPED{{(/|\\\\)}}@__swiftmacro
@Observable
class C: Observable {}

// CHECK-FILES: @__swiftmacro_
