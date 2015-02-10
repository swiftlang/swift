// Run test ASTSection.swift, with separate compile and link steps.

// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: %target-swift-frontend -c -emit-module -o %t %S/ASTSection.swift

// Test the inline section mechanism.
// RUN: ld %t/ASTSection.o -sectcreate __SWIFT __ast %t/ASTSection.swiftmodule -o %t/ASTSection.dylib -L%platform-module-dir -dylib -lSystem
// RUN: %lldb-moduleimport-test %t/ASTSection.dylib | FileCheck %S/ASTSection.swift

// Test the symbol table entry.
// RUN: ld %t/ASTSection.o -add_ast_path %t/ASTSection.swiftmodule -o %t/ASTSection.dylib -L%platform-module-dir -dylib -lSystem
// RUN: %lldb-moduleimport-test %t/ASTSection.dylib | FileCheck %S/ASTSection.swift

// REQUIRES: OS=macosx
// REQUIRES: ld-add_ast_path
