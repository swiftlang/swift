// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: rm -rf %t && mkdir %t

// RUN: %target-build-swift -emit-executable %s -g -o %t/ASTSection -emit-module
// RUN: %lldb-moduleimport-test %t/ASTSection | FileCheck %s

// RUN: cp %S/Inputs/serialized-objc-header.h %t
// RUN: %target-build-swift -emit-executable %s -g -o %t/ASTSection-with-ObjC -import-objc-header %t/serialized-objc-header.h -DOBJC -module-name ASTSection -emit-module
// RUN: %lldb-moduleimport-test %t/ASTSection-with-ObjC | FileCheck %s

// RUN: rm %t/serialized-objc-header.h
// RUN: %lldb-moduleimport-test %t/ASTSection-with-ObjC | FileCheck %s

// RUN: %target-build-swift -emit-executable %s -gline-tables-only -o %t/ASTSection -emit-module
// RUN: %lldb-moduleimport-test %t/ASTSection | FileCheck %s --allow-empty --check-prefix=LINETABLE-CHECK

// UNSUPPORTED: OS=linux-gnu

// Test ASTSection_linker.swift builds this code 
// with separate compile and link steps.

// A type that should be serialized.
class Foo {
init() { }
func bar() -> Int { return 42 }
}

// Some toplevel code that should not be serialized.
var foo: Foo = Foo()
Double(foo.bar())

#if OBJC
func objCUser(obj: ObjCClass) {}
#endif

// CHECK: Loaded module ASTSection from
// CHECK: - Target: {{.+}}-{{.+}}-{{.+}}
// CHECK: Importing ASTSection... ok!

// LINETABLE-CHECK-NOT: ASTSection

