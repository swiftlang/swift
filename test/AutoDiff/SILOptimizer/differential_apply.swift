// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s
// REQUIRES: swift_in_compiler

import _Differentiation

@differentiable(reverse)
@_silgen_name("test_f")
// Check that (differentiable) closure apply is optimized out 
// CHECK-LABEL: test_f : $@convention(thin) (@guaranteed Array<Double>) -> Double
// CHECK-NOT: differentiable_function [parameters 0] [results 0]
func f(array: [Double]) -> Double {
  var array = array
  array.update(at: 1,
    byCalling: {
      (element: inout Double) in
      let initialElement = element;
      element *= initialElement
    }
  )
  
  return 0.0
}

public func valueWithPullback<T>(at x: T, of f: @differentiable(reverse) (inout T) -> Void) -> (value: Void, pullback: (inout T.TangentVector) -> Void) {fatalError()}
public func pullback<T>(at x: T, of f: @differentiable(reverse) (inout T) -> Void) -> (inout T.TangentVector) -> Void {return valueWithPullback(at: x, of: f).pullback}

public extension Array {
  @differentiable(reverse)
  mutating func update(at index: Int,
    byCalling closure: @differentiable(reverse) (inout Element) -> Void) where Element: Differentiable {
    closure(&self[index])
  }
}

public extension Array where Element: Differentiable {
    @derivative(of: update(at:byCalling:))
    mutating func vjpUpdate(at index: Int, byCalling closure: @differentiable(reverse) (inout Element) -> Void) -> (value: Void, pullback: (inout Self.TangentVector) -> Void) {
        let closurePullback = pullback(at: self[index], of: closure)
        return (value: (), pullback: { closurePullback(&$0.base[index]) })
    }
}

public struct D<I: Equatable, D> {
  public subscript(_ index: I) -> D? {
    get {fatalError()}
    set {fatalError()}
  }
}
