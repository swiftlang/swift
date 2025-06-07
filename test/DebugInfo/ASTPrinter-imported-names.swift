// REQUIRES: executable_test
// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: echo '$sSo3FooVD' > %t/list
// RUN: %target-build-swift -emit-executable %s -g -o %t/a.out -I %S/Inputs \
// RUN:   -module-name ASTPrinter -emit-module
// RUN: %lldb-moduleimport-test -qualify-types \
// RUN:   -type-from-mangled=%t/list %t/a.out | %FileCheck %s

// This name should come out fully qualified.
// CHECK: ClangModule.Foo
import ClangModule
let foo = Foo()
