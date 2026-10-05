// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test -print-indexed-symbols -module-to-print Library -source-filename %t/main.swift -plugin-path %swift-plugin-dir -I %t/Inputs | %FileCheck %s

// FIXME: Both ClangModuleUnit and `getTopLevelDeclsWithAuxiliaryDecls` expand
// the macro here.
// CHECK: | function(public)/Swift | readWidgets(_:_:) | c:@F@readWidgets | Def
// CHECK-COUNT-2: | function(public)/Swift | readWidgets(_:) | {{.*}} | Def
// CHECK-NOT: | function(public)/Swift | readWidgets(_:) | {{.*}} | Def

//--- Inputs/module.modulemap
module Library {
  header "library.h"
  export *
}

//--- Inputs/library.h
__attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"count\"), typeMappings: [\"int * _Nonnull\" : \"UnsafeMutablePointer<CInt>\"])")))
void readWidgets(int count, int * _Nonnull widgets);

//--- main.swift
import Library
