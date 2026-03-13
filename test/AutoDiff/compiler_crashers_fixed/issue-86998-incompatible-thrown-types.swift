// RUN: %target-swift-frontend -typecheck -verify %s

// https://github.com/swiftlang/swift/issues/84365
// Ensure we check thrown types when resolving custom derivatives
import _Differentiation

extension Array where Element: Differentiable {
    // expected-note @+2 {{candidate instance method does not have type equal to or less constrained than '<Element, Result, E where Element : Differentiable, Result : Differentiable, E : Error> (Array<Element>) -> (@differentiable(reverse) (Element) throws(E) -> Result) throws -> [Result]'}}
    @differentiable(reverse, wrt: self)
    public func differentiableThrowingMap<Result: Differentiable, E: Error>(
      _ body: @differentiable(reverse) (Element) throws(E) -> Result
    ) throws(E) -> [Result] {
        try map(body)
    }

  // expected-error @+1 {{referenced declaration 'differentiableThrowingMap' could not be resolved}}
    @derivative(of: differentiableThrowingMap)
    public func _vjpDifferentiableThrowingMap<Result: Differentiable, E: Error>(
        _ body: @differentiable(reverse) (Element) throws(E) -> Result
  ) throws -> ( // succeeds when turned into throws(E)
        value: [Result],
        pullback: (Array<Result>.TangentVector) -> Array.TangentVector
    ) {
        let count = self.count
        var values: [Result] = []
        var pullbacks: [(Result.TangentVector) -> Element.TangentVector] = []
        values.reserveCapacity(count)
        pullbacks.reserveCapacity(count)
        for x in self {
            let (y, pb) = try valueWithPullback(at: x, of: body)
            values.append(y)
            pullbacks.append(pb)
        }
        func pullback(_ tans: Array<Result>.TangentVector) -> Array.TangentVector {
            .init(zip(tans.base, pullbacks).map { tan, pb in pb(tan) })
        }
        return (value: values, pullback: pullback)
    }
}
