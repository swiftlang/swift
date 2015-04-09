// Run test ASTSection.swift, with separate compile and link steps.

// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: %target-swift-frontend -c -sdk /fake/sdk/path -Xcc -DA -Xcc -DB -emit-module -o %t %S/ASTSection.swift
// RUN: %swift-ide-test -test-CompilerInvocation-from-module -source-filename=%t/ASTSection.swiftmodule

// Test the inline section mechanism.
// RUN: %target-swiftc_driver %t/ASTSection.o -Xlinker -sectcreate -Xlinker __SWIFT -Xlinker __ast -Xlinker %t/ASTSection.swiftmodule -o %t/ASTSection.dylib
// RUN: %lldb-moduleimport-test %t/ASTSection.dylib | FileCheck %s

// Test the symbol table entry.
// RUN: %target-swiftc_driver %t/ASTSection.o -Xlinker -add_ast_path -Xlinker %t/ASTSection.swiftmodule -o %t/ASTSection.dylib
// RUN: %lldb-moduleimport-test %t/ASTSection.dylib | FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: ld-add_ast_path

// CHECK: Loaded module ASTSection from
// CHECK: - Target: {{.+}}-{{.+}}-{{.+}}
// CHECK: - SDK path: /fake/sdk/path{{$}}
// CHECK: - -Xcc options: -DA -DB
// CHECK: Importing ASTSection... ok!
