// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -parse-stdlib -o %t %S/Inputs/alias_builtin.swift
// RUN: llvm-bcanalyzer %t/alias_builtin.swiftmodule | FileCheck %s
// RUN: %swift -I=%t -parse %s -verify

// CHECK-NOT: UnknownCode

import alias_builtin

var a : TheBuiltinInt64

// Check that it really is Builtin.Int64.
var wrapped = Int64(a) // okay
var badWrapped = Int32(a) // expected-error{{could not find an overload for 'init' that accepts the supplied arguments}}
