// RUN: %target-swift-frontend -sil-verify-all -Xllvm -sil-print-types -emit-sil -O -g %s | %FileCheck %s

// REQUIRES: swift_in_compiler

// Fix for https://github.com/apple/swift/issues/62608
// We need to emit separate debug info location for different adjoint buffers
// created for the single input variable

import _Differentiation

public extension Array {
    @inlinable
    @differentiable(reverse)
    mutating func update(at index: Int, byCalling closure: @differentiable(reverse) (inout Element) -> Void) where Element: Differentiable {
        closure(&self[index])
    }
}

public func valueWithPullback<T>(
    at x: T, of f: @differentiable(reverse) (inout T) -> Void
) -> (value: Void, pullback: (inout T.TangentVector) -> Void) {
    @differentiable(reverse)
    func nonInoutWrappingFunction(_ t: T) -> T {
        var t = t
        f(&t)
        return t
    }
    let nonInoutPullback = pullback(at: x, of: nonInoutWrappingFunction)
    return ((), { $0 = nonInoutPullback($0) })
}

@inlinable
public func pullback<T>(
    at x: T, of f: @differentiable(reverse) (inout T) -> Void
) -> (inout T.TangentVector) -> Void {
    return valueWithPullback(at: x, of: f).pullback
}

// CHECK-LABEL: sil private @$s4main19testUpdateByCallingyyKF8fOfArrayL_5arraySdSaySdG_tFySdzcfU_TJpSUpSr :
// CHECK: alloc_stack $Double, var, name "derivative of 'element' in scope at {{.*}} (scope #3)"
// CHECK: debug_value %{{.*}} : $Builtin.FPIEEE64, var, (name "derivative of 'element' in scope at {{.*}} (scope #{{.*}})"

public extension Array where Element: Differentiable {
    @inlinable
    @derivative(of: update(at:byCalling:))
    mutating func vjpUpdate(
        at index: Int,
        byCalling closure: @differentiable(reverse) (inout Element) -> Void
    )
    ->
    (value: Void, pullback: (inout Self.TangentVector) -> Void)
    {
        let closurePullback = pullback(at: self[index], of: closure)
        return (value: (), pullback: { closurePullback(&$0.base[index]) })
    }
}

func testUpdateByCalling() throws {
    @differentiable(reverse)
    func fOfArray(array: [Double]) -> Double {
        var array = array
        var result = 0.0
        for i in withoutDerivative(at: 0 ..< array.count) {
            array.update(at: i, byCalling: { (element: inout Double) in
                let initialElement = element
                for _ in withoutDerivative(at: 0 ..< i) {
                    element *= initialElement
                }
            })
            result += array[i]
        }
        return result
    }
    
    let array = [Double](repeating: 1.0, count: 3)
    let expectedGradientOfFOfArray = [1.0, 2.0, 3.0]
    let obtainedGradientOfFOfArray = gradient(at: array, of: fOfArray).base
}

