// Checks that macro-expanded members are dumped and that their source ranges
// contain the appropriate buffer ID.

// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/json_ast_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/json_ast_macro_library.swiftmodule %S/Inputs/json_ast_macro_library.swift -module-name json_ast_macro_library -load-plugin-library %t/%target-library-name(MacroDefinition)
// RUN: %target-swift-frontend -target %target-swift-5.9-abi-triple -I %t -load-plugin-library %t/%target-library-name(MacroDefinition) -parse-as-library -dump-ast -dump-ast-format json %s -module-name main -o - | %FileCheck %s

import json_ast_macro_library

@InjectMember
struct X {
    var y: Int
}

// CHECK:      "_kind":"pattern_binding_decl"
// CHECK-SAME: "buffer_id":"@__swiftmacro_4main1X12InjectMemberfMm_.swift"
// CHECK-SAME: "name":"_macroInjectedMember"
