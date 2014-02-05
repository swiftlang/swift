// Run test ASTSection.swift, with separate compile and link steps.

// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: %swift_driver -frontend -c -emit-module -o %t %s
// RUN: %ld %t/ASTSection.o -sectcreate __SWIFT __ast %t/ASTSection.swiftmodule -o %t/ASTSection.dylib -L%libdir/swift/macosx -dylib -lSystem
// RUN: %lldb-moduleimport-test %t/ASTSection.dylib | FileCheck %s
// REQUIRES: macosx
