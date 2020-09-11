@inlinable
@differentiable(where T: Differentiable)
public func identity<T>(_ x: T) -> T { x }

public func foo<T: Differentiable>(_ f: @differentiable (T) -> T = identity) -> T {
    fatalError()
}
