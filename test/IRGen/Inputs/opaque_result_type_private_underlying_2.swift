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


public protocol P {}

private struct PrivateSome : P {}
public func getSome() -> some P {
  return PrivateSome()
}

@propertyWrapper
public struct Wrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue v: T) {
    wrappedValue = v
  }
}

public struct R<V, T: P>: P {
  @Wrapper private var privateState = PrivateState()
  var x: T? = nil

  public init(_ v: V.Type, _ t: T) {
    x = t
  }

  public mutating func modify() {
    x = nil
  }
}

private extension R {
  struct PrivateState {
    var x = 0
  }
}
