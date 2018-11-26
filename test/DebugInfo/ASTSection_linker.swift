// Run test ASTSection.swift, with separate compile and link steps.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -c -sdk /fake/sdk/path -Xcc -DA -Xcc -DB -emit-module -o %t %S/ASTSection.swift
// RUN: %swift-ide-test -test-CompilerInvocation-from-module -source-filename=%t/ASTSection.swiftmodule

// Test the inline section mechanism.
// RUN: %target-ld %t/ASTSection.o -sectcreate __SWIFT __ast %t/ASTSection.swiftmodule -o %t/ASTSection.dylib -dylib -lSystem -lobjc
// RUN: %lldb-moduleimport-test -verbose %t/ASTSection.dylib | %FileCheck %s

// Test the symbol table entry.
// RUN: %target-ld %t/ASTSection.o -add_ast_path %t/ASTSection.swiftmodule -o %t/ASTSection.dylib -dylib -lSystem -lobjc
// RUN: %lldb-moduleimport-test -verbose %t/ASTSection.dylib | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK: - Swift Version: {{.+}}.{{.+}}
// CHECK: - Target: {{.+}}-{{.+}}-{{.+}}
// CHECK: - SDK path: /fake/sdk/path{{$}}
// CHECK: - -Xcc options: -working-directory {{.+}} -DA -DB
// CHECK: Importing ASTSection... ok!
