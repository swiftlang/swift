import External2

@usableFromInline
@inline(never)
func preventInlining() {}

// When specializing the opaque result type for this function we should not
// specialize the opaque result type of the recursive invocation.
@inlinable
public func inlinableExternalResilientCallsResilient() -> some ExternalP2 {
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  return externalResilient()
}

// In this case we should look through the recursion.
@inlinable
public func inlinableExternalResilientCallsInlinableExternalResilient() -> some ExternalP2 {
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
  preventInlining()
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
  public var inlineableProperty : some ExternalP2 {
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    return r.computedProperty
  }

  @inlinable
  public var inlineablePropertyCallsResilientInlineable : some ExternalP2 {
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    preventInlining()
    return r.inlineableProperty
  }
}
