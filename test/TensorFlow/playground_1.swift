// Test that the playground transform + tensor operations play well together,
// that we are able to promote things to graph properly, that the playground
// logger gets the right stuff, and we're not generating implicit sends
// unexpectedly.

// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -debugger-support -dump-ast -playground %t/main.swift %S/PlaygroundsRuntime.swift
// RUN: %target-swift-frontend -debugger-support -Xllvm -tf-dump-intermediates -O -emit-sil -playground %t/main.swift %S/PlaygroundsRuntime.swift
// RUN: %target-swift-frontend -debugger-support -Xllvm -tf-dump-intermediates -O -emit-sil -playground %t/main.swift %S/PlaygroundsRuntime.swift -verify | %FileCheck %s

import TensorFlow

// Check that logging is happening for simple things.
let x = 12345678  // a distinctive number to filecheck for.

// CHECK: [[INT:%.*]] = integer_literal $Builtin.Int64, 12345678
// CHECK: [[INT2:%.*]] = struct $Int ([[INT]] : $Builtin.Int64)
// CHECK: store [[INT2]] to [[INTP:%.*]] : $*Int
// CHECK: [[INT:%.*]] = load [[INTP]] : $*Int
// CHECK: [[INTP:%.*]] = alloc_stack $Int
// CHECK: store [[INT]] to [[INTP]] : $*Int
// CHECK: [[LOGFN:%.*]] = function_ref @{{.*}}__builtin_log_with_id{{.*}}
// CHECK: apply [[LOGFN]]<Int>([[INTP]],


let a = Tensor<Float>([1,2,3])
var b = a+a
b -= a


func someLocalFunctionThatShouldBeForceInlined() {
  b *= a
}


// FIXME: Enable this when we handle function calls in playgrounds correctly.
//someLocalFunctionThatShouldBeForceInlined()

// CHECK-LABEL: --- TFPartition Accelerator Result: main
// CHECK: sil private @main.tf : $@callee_owned ()
// CHECK:   graph_op "Const"
// CHECK:   builtin "__tfop_Add,
// CHECK:   builtin "__tfop_Sub,
// CHECK:   return

// CHECK-LABEL: --- XLA CFG Canonicalize: main
