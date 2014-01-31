// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: %swift_driver -target armv7-apple-ios7.0 %s -g -o %t/ASTSection_arm
// RUN: %lldb-moduleimport-test %t/ASTSection_arm | FileCheck %s
// REQUIRES: ARM

// A type that should be serialized.
class Foo {
init() { }
func bar() -> Int { return 42 }
}

// Some toplevel code that should not be serialized.
var foo: Foo = Foo()
println(foo.bar())

// CHECK: Importing ASTSection_arm... ok!

