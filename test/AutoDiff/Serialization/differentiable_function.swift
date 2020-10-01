// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: llvm-bcanalyzer %t/differentiable_function.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -enable-sil-verify-all %t/differentiable_function.swiftmodule -o - | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

import _Differentiation

func a(_ f: @differentiable (Float) -> Float) {}
// CHECK: func a(_ f: @differentiable (Float) -> Float)

func b(_ f: @differentiable(linear) (Float) -> Float) {}
// CHECK: func b(_ f: @differentiable(linear) (Float) -> Float)

func c(_ f: @differentiable (Float, @noDerivative Float) -> Float) {}
// CHECK: func c(_ f: @differentiable (Float, @noDerivative Float) -> Float)
