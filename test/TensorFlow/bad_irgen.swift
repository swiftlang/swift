// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-ir %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-ir %s -verify | %FileCheck %s

import TensorFlow

@_silgen_name("_testTensor")
public func testTensor() {
  // We invoke #tfop directly here to check the generated IR code.
  // The #tfop builtins used in Tensor ops are compiled when building the swift
  // library module, so their IR code is not easily accessible during unit
  // testing.
  _ = #tfop("Const", ":t", dtype: Float.self, value$tensor: 1.0) as TensorHandle<Float>
}

testTensor()

// CHECK: define protected swiftcc void @_testTensor()
// CHECK: call i32 (i8*, ...) @printf
// CHECK-NEXT: call void @abort()
