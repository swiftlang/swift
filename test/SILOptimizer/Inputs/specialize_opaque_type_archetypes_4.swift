import External2

// When specializing the opaque result type for this function we should not
// specialize the opaque result type of the recursive invocation.
@inlinable
@inline(never)
public func inlinableExternalResilientCallsResilient() -> some ExternalP2 {
  return externalResilient()
}

// In this case we should look through the recursion.
@inlinable
@inline(never)
public func inlinableExternalResilientCallsInlinableExternalResilient() -> some ExternalP2 {
  return inlinableExternalResilient()
}

public struct ResilientContainer2 {
  @usableFromInline
  var r = ResilientContainer()

  public init() {}

  public var computedProperty : some ExternalP2 {
    return r.computedProperty
  }

  @inlinable
  @inline(never)
  public var inlineableProperty : some ExternalP2 {
    return r.computedProperty
  }

  @inlinable
  @inline(never)
  public var inlineablePropertyCallsResilientInlineable : some ExternalP2 {
    return r.inlineableProperty
  }
}
