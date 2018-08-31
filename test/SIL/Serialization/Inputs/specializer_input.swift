
public typealias Int = Builtin.Int32

@_fixed_layout
public struct Container<V> {
  @inlinable
  @inline(never)
  public func doSomething() {}

  @inlinable
  @inline(never)
  public init() {}
}

