// RUN: rm -rf %t
// RUN: mkdir %t
//
// RUN: %target-build-swift %s -g -o %t/ASTSection
// RUN: lldb-moduleimport-test %t/ASTSection | FileCheck %s
//
// FIXME: lldb-moduleimport-test does not support 32-bit executables
// rdar://16244944
// XFAIL: PTRSIZE=32
//
// Test ASTSection_linker.swift builds this code 
// with separate compile and link steps.

// A type that should be serialized.
class Foo {
init() { }
func bar() -> Int { return 42 }
}

// Some toplevel code that should not be serialized.
var foo: Foo = Foo()
println(foo.bar())

// CHECK: Importing ASTSection... ok!

