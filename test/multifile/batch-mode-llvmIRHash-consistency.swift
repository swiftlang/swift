// Ensure that the LLVMIR hash of the 2nd compilation in batch mode
// is consistent no matter if the first one generates code or not.

// This test fails in some configurations.
// REQUIRES: rdar_62338337

// RUN: %empty-directory(%t)
// RUN: echo 'public enum E: Error {}' >%t/main.swift
// RUN: echo >%t/other.swift
// RUN: touch -t 201901010101 %t/*.swift

// RUN: cd %t; %target-swift-frontend -c -g -enable-batch-mode -module-name main -primary-file ./main.swift -primary-file other.swift

// RUN: cp %t/main{,-old}.o
// RUN: cp %t/other{,-old}.o
// RUN: echo 'public enum E: Error {}' >%t/main.swift
// RUN: echo >%t/other.swift

// RUN: cd %t; %target-swift-frontend -c -g -enable-batch-mode -module-name main -primary-file ./main.swift -primary-file other.swift

// Ensure that code generation was not performed for other.swift

// RUN: ls -t %t | %FileCheck %s

// CHECK: other.swift
// CHECK: other.o
