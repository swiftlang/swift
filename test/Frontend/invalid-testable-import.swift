// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/single_int.swiftmodule %S/Inputs/single_int.swift
// RUN: not %target-swift-frontend -typecheck %s -I %t 2>&1 | %FileCheck %s

@testable import single_int // CHECK: module 'single_int' was not compiled for testing

x = 8

// Note: extra newlines below ensure that context printing doesn't show the
// lines that we shouldn't see.


// CHECK-NOT: cannot find 'x' in scope
