// RUN: %target-swift-frontend -emit-sil -O %s

// Fix for https://github.com/apple/swift/issues/65073:
// We need to ensure we are not dropping the substitution map during the
// derivative type calculation in sil combiner that causes errors in the
// subsequent optimizations as we're ending with invalid "recursive" 
// subsitutions

import _Differentiation;

@differentiable(reverse)
func f(array: [Double]) -> Double {
    var array = array
    for index in withoutDerivative(at: 0 ..< array.count) {array.update(at: index, byCalling: { (element: inout Double) in let initialElement = element; for _ in withoutDerivative(at: 0 ..< index) {element *= 
initialElement}})}
    return 0.0
}
public func valueWithPullback<T>(at x: T, of f: @differentiable(reverse) (inout T) -> Void) -> (value: Void, pullback: (inout T.TangentVector) -> Void) {fatalError()}
public func pullback<T>(at x: T, of f: @differentiable(reverse) (inout T) -> Void) -> (inout T.TangentVector) -> Void {return valueWithPullback(at: x, of: f).pullback}
public extension Array {@differentiable(reverse) mutating func update(at index: Int, byCalling closure: @differentiable(reverse) (inout Element) -> Void) where Element: Differentiable {closure(&self[index])}}
public extension Array where Element: Differentiable {
    @derivative(of: update(at:byCalling:))
    mutating func vjpUpdate(at index: Int,byCalling closure: @differentiable(reverse) (inout Element) -> Void) -> (value: Void, pullback: (inout Self.TangentVector) -> Void){
        let closurePullback = pullback(at: self[index], of: closure)
        return (value: (), pullback: { closurePullback(&$0.base[index]) })
    }
}
public struct D<I: Equatable, D> {public subscript(_ index: I) -> D? {get {fatalError()} set {fatalError()}}}
