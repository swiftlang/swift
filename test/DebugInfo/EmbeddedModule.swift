// RUN: %swift -triple x86_64-apple-darwin10 %s -S -g -o - | FileCheck %s
// CHECK: .section	__DWARF,__apple_swiftast,regular,debug
// CHECK: .quad {{[0-9]+}}
// CHECK: EmbeddedModule.swift
// CHECK: .quad {{[0-9]+}}
// CHECK: .asciz

// A type that should be serialized.
class Foo {
      func bar() -> Int { return 42; }
}

// Some toplevel code that should not be serialized.
var foo: Foo;
println(foo.bar())
