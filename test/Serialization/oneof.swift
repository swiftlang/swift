// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/def_oneof.swiftmodule %S/Inputs/def_oneof.swift
// RUN: llvm-bcanalyzer %t/def_oneof.swiftmodule | FileCheck %s
// RUN: %swift -parse -I=%t %s -o /dev/null

// CHECK-NOT: FALL_BACK_TO_TRANSLATION_UNIT

import def_oneof

var a : Basic
a = .Untyped
a.doSomething()
a = .HasType(4)
a.doSomething()

var g = Generic.Left(false)
g = .Right(true)

var lazy = Lazy.Thunk({ 42 })
var comp : Computable = lazy
comp.compute()
lazy.compute()
