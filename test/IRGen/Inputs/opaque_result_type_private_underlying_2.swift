public protocol A {
  associatedtype Assoc
  func bindAssoc() -> Assoc
}

public protocol Q {}

public struct PublicS : Q {
  public init() {}
}

internal struct InternalS : Q {
  init() {}
}

fileprivate struct PrivateS : Q {
  init() {}
}

public struct PrivateUnderlying : A {
  public init() {}
  public func bindAssoc() -> some Q {
    return PrivateS()
  }
}

public struct InternalUnderlying : A {
  public init() {}
  public func bindAssoc() -> some Q {
    return InternalS()
  }
}

public struct PublicUnderlying : A {
  public init() {}
  public func bindAssoc() -> some Q {
    return PublicS()
  }
}

public struct PublicUnderlyingInlinable : A {
  public init() {}
	@inlinable
  public func bindAssoc() -> some Q {
    return PublicS()
  }
}
