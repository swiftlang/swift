// REQUIRES: swift_swift_parser, executable_test
// REQUIRES: swift_feature_MacrosOnImports

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// Build the macro library to give us access to ExpandTypeError.
// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/macro_library.swiftmodule %S/Inputs/macro_library.swift -module-name macro_library -load-plugin-library %t/%target-library-name(MacroDefinition)

// FIXME: we should typecheck these macro expansions before silgen
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -swift-version 5 -enable-experimental-feature MacrosOnImports -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name ErrorModuleUser %s -I %t

// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -swift-version 5 -enable-experimental-feature MacrosOnImports -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name ErrorModuleUser %s -I %t 2>&1 | %FileCheck %s

import ImportedMacroError
import macro_library

foo(42)

// CHECK: error: cannot find 'callToMissingFunction' in scope
