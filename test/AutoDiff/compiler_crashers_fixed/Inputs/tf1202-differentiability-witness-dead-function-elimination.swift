import _Differentiation

@inlinable
@differentiable(reverse where T: Differentiable)
public func identity<T>(_ x: T) -> T { x }

public func foo<T: Differentiable>(_ f: @differentiable(reverse) (T) -> T = identity) -> T {
  fatalError()
}
