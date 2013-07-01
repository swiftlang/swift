// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -parse-stdlib -o %t/alias_builtin.swiftmodule %S/Inputs/alias_builtin.swift
// RUN: %swift -I=%t -parse %s -verify

import alias_builtin

var a : TheBuiltinInt64

// Check that it really is Builtin.Int64.
var wrapped = Int64(a) // okay
var badWrapped = Int32(a) // expected-error{{expression does not type-check}}
