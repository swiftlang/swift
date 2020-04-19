// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -diagnostic-style=swift -I %S/Inputs/ -typecheck %s -module-cache-path %t.mcp 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

import RenamedObjc

let foo = SwiftNameTest(g: "")

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}Inputs{{[/\]+}}RenamedObjc.h:7:1
// CHECK: 7 | + (instancetype)g:(id)x outParam:(int *)foo SWIFT_NAME(init(g:));
// CHECK:   | ^ warning: too few parameters in swift_name attribute (expected 2; got 1)
// CHECK:   | ^ note: please report this issue to the owners of 'RenamedObjc'


// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-diags-in-clang-buffer.swift:8:28
// CHECK: 7 |
// CHECK: 8 | let foo = SwiftNameTest(g: "")
// CHECK:   |                            ~~
// CHECK:   |                            ^ error: argument passed to call that takes no arguments
