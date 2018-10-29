// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t %S/Inputs/alias_builtin.swift
// RUN: llvm-bcanalyzer %t/alias_builtin.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -I %t -typecheck %s -verify

// CHECK-NOT: UnknownCode

import alias_builtin

var a : TheBuiltinInt64

// Check that it really is Builtin.Int64.
var wrapped = Int64(a) // okay
var badWrapped = Int32(a) // expected-error{{initializer 'init(_:)' requires that 'TheBuiltinInt64' conform to 'BinaryInteger'}}
