// RUN: %empty-directory(%t)
// RUN: %target-build-swift -c -force-single-frontend-invocation -parse-as-library -parse-stdlib -module-name Swift -emit-module -emit-module-path %t/Swift.swiftmodule -o %t/Swift.o %S/Inputs/Swift.swift
// RUN: ls %t/Swift.swiftmodule
// RUN: ls %t/Swift.swiftdoc
// RUN: ls %t/Swift.o
// RUN: %target-clang -x c -c %S/Inputs/RuntimeStubs.c -o %t/RuntimeStubs.o
// RUN: %target-build-swift -I %t -module-name main -o %t/hello %S/Inputs/main.swift %t/Swift.o %t/RuntimeStubs.o
// RUN: %target-codesign %t/hello
// RUN: %target-run %t/hello | %FileCheck %s
// REQUIRES: executable_test
// CHECK: Hello

