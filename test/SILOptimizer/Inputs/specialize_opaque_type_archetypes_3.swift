public protocol ExternalP2 {
  func myValue3() -> Int64
}

extension Int64 : ExternalP2 {
  public func myValue3() -> Int64 {
    return self + 3
  }
}

public func externalResilient() -> some ExternalP2 {
  return Int64(6)
}

@inlinable
@inline(never)
public func inlinableExternalResilient() -> some ExternalP2 {
  return Int64(6)
}

public struct ResilientContainer {
  @usableFromInline
  var x = Int64(0)

  public init() {}

  public var computedProperty : some ExternalP2 {
    return x
  }

  @inlinable
  @inline(never)
  public var inlineableProperty : some ExternalP2 {
    return x
  }

  @_alwaysEmitIntoClient
  @inline(never)
  public var inlineableProperty2 : some ExternalP2 {
    return x
  }


  @inlinable
  public func inlineableContext() {
    let x = computedProperty
    print(x)
  }
}

public struct WrapperP2<Wrapped: ExternalP2>: ExternalP2 {
  public init(_ wrapped: Wrapped) {}
  public func myValue3() -> Int64 { 0 }
}

public func externalResilientWrapper<Wrapped: ExternalP2>(_ wrapped: Wrapped) -> some ExternalP2 {
  return WrapperP2(wrapped)
}

@inlinable
@inline(never)
public func inlinableExternalResilientWrapper<Wrapped: ExternalP2>(_ wrapped: Wrapped) -> some ExternalP2 {
  return WrapperP2(wrapped)
}

