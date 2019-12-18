// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: llvm-bcanalyzer %t/derivative_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -disable-sil-linking -enable-sil-verify-all %t/derivative_attr.swiftmodule -o - | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

struct DerivativeOfSpecialMethods: Differentiable {
  var x: Float

  init(_ x: Float) { self.x = x }
  subscript(_ i: Int) -> Float { x }

  // CHECK: @derivative(of: init, wrt: x)
  // CHECK-NEXT: static func dInit(_ x: Float)
  @derivative(of: init(_:))
  static func dInit(_ x: Float) -> (value: Self, pullback: (TangentVector) -> Float) {
    fatalError()
  }

  // CHECK: @derivative(of: subscript, wrt: self)
  // CHECK-NEXT: func dSubscript(_ i: Int)
  @derivative(of: subscript(_:))
  func dSubscript(_ i: Int) -> (value: Float, pullback: (Float) -> TangentVector) {
    fatalError()
  }
}
