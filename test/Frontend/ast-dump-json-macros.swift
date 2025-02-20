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
// CHECK:      "_kind":"struct_decl",
// CHECK-SAME: "usr":"s:4main1XV",
// CHECK-SAME: "_kind":"custom_attr",
// CHECK-SAME: "macro":{"_kind":"decl_ref","base_name":"InjectMember","decl_usr":"s:22json_ast_macro_library12InjectMemberyycfm","type_usr":"$syycD"}
// CHECK-SAME: "_kind":"pattern_binding_decl"
// CHECK-SAME: "buffer_id":"@__swiftmacro_4main1X12InjectMemberfMm_.swift"
// CHECK-SAME: "name":"_macroInjectedMember"

struct Z {
    #injectFreestanding
}
// NOTE: For freestanding members (as opposed to top-level), we get the expanded
// decl before we see the MacroExpansionDecl.
// CHECK-SAME: "_kind":"struct_decl",
// CHECK-SAME: "usr":"s:4main1ZV20FixedNameFreestanderV",
// CHECK-SAME: "buffer_id":"@__swiftmacro_4main0033astdumpjsonmacrosswift_GwAFheaeGafMX{{[0-9]+}}_{{[0-9]+}}_33_{{[0-9A-F]+}}Ll18injectFreestandingfMf_.swift"
// CHECK-SAME: "_kind":"macro_expansion_decl",
// CHECK-SAME: "auxiliary_decl_usrs":["s:4main1ZV20FixedNameFreestanderV"]
// CHECK-SAME: "macro":{"_kind":"decl_ref","base_name":"injectFreestanding","decl_usr":"s:22json_ast_macro_library18injectFreestandingyycfm","type_usr":"$syycD"}

@InjectPeer
struct ThisWillBePeered {}
// CHECK-SAME: "_kind":"struct_decl",
// CHECK-SAME: "usr":"s:4main16ThisWillBePeeredV",
// CHECK-SAME: "auxiliary_decl_usrs":["s:4main13FixedNamePeerV"],
// CHECK-SAME: "_kind":"custom_attr",
// CHECK-SAME: "macro":{"_kind":"decl_ref","base_name":"InjectPeer","decl_usr":"s:22json_ast_macro_library10InjectPeeryycfm","type_usr":"$syycD"}
// CHECK-SAME: "_kind":"struct_decl",
// CHECK-SAME: "usr":"s:4main13FixedNamePeerV",
// CHECK-SAME: "buffer_id":"@__swiftmacro_4main16ThisWillBePeered10InjectPeerfMp_.swift"

#injectFreestanding
// CHECK-SAME: "_kind":"macro_expansion_decl",
// CHECK-SAME: "auxiliary_decl_usrs":["s:4main20FixedNameFreestanderV"],
// CHECK-SAME: "macro":{"_kind":"decl_ref","base_name":"injectFreestanding","decl_usr":"s:22json_ast_macro_library18injectFreestandingyycfm","type_usr":"$syycD"}
// CHECK-SAME: "_kind":"struct_decl",
// CHECK-SAME: "usr":"s:4main20FixedNameFreestanderV",
// CHECK-SAME: "buffer_id":"@__swiftmacro_4main0033astdumpjsonmacrosswift_GwAFheaeGafMX{{[0-9]+}}_{{[0-9]+}}_33_{{[0-9A-F]+}}Ll18injectFreestandingfMf_.swift"
