// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -enable-experimental-differentiable-programming -o %t
// RUN: llvm-bcanalyzer %t/differentiation.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -disable-sil-linking -enable-sil-verify-all %t/differentiation.swiftmodule -enable-experimental-differentiable-programming -o - | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

func a(_ f: @differentiable (Float) -> Float) {}
// CHECK: func a(_ f: @differentiable (Float) -> Float)

func b(_ f: @differentiable(linear) (Float) -> Float) {}
// CHECK: func b(_ f: @differentiable(linear) (Float) -> Float)
