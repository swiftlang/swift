
public typealias Int = Builtin.Int32

@_fixed_layout
public struct Container<V> {
  @_inlineable
  @inline(never)
  public func doSomething() {}

  @_inlineable
  @inline(never)
  public init() {}
}

