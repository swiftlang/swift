// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: %swift_driver -frontend -c -emit-module -o %t %s
// RUN: %ld %t/ASTSection.o -sectcreate __SWIFT __ast %t/ASTSection.swiftmodule -o %t/ASTSection.dylib -L%libdir/swift/macosx -dylib -lSystem
// RUN: %lldb-moduleimport-test %t/ASTSection.dylib | FileCheck %s

// RUN: %swift_driver -target x86_64-apple-darwin10 %s -g -o %t/ASTSection
// RUN: %lldb-moduleimport-test %t/ASTSection | FileCheck %s
// REQUIRES: X86

// A type that should be serialized.
class Foo {
init() { }
func bar() -> Int { return 42 }
}

// Some toplevel code that should not be serialized.
var foo: Foo = Foo()
println(foo.bar())

// CHECK: Importing ASTSection... ok!

