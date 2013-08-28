// RUN: %swift -triple x86_64-apple-darwin10 %s -S -g -o - | FileCheck %s
// CHECK: .section	__DWARF,__apple_ast,regular,debug
// CHECK: [[SECTION:.*]]:
// Version
// CHECK: .long 1
// Number of modules
// CHECK: .long 1
// Bitstream offset
// CHECK: .quad	[[BITSTREAM:.*]]-[[SECTION]]
// Bitstream bytesize
// CHECK: .quad
// Name offset
// CHECK: .quad	[[NAME:.*]]-[[SECTION]]
// Language
// CHECK: .long	15
// Flags
// CHECK: .long
// CHECK: [[NAME]]:
// CHECK: .long
// CHECK: .ascii{{.*}}ASTStreamer.swift
// CHECK: [[BITSTREAM]]:
// CHECK: .quad
// CHECK: .quad

// A type that should be serialized.
class Foo {
      func bar() -> Int { return 42; }
}

// Some toplevel code that should not be serialized.
var foo: Foo;
println(foo.bar())
