public struct MarkerForNonOpenSubscripts { public init() {} }
public struct MarkerForOpenSubscripts { public init() {} }

public class ExternalNonOpenClass {
  init() {}
  public func nonOpenMethod() {}
  public var nonOpenProperty: Int = 0
  public subscript(index: MarkerForNonOpenSubscripts) -> Int {
    get { return 0 }
    set {}
  }
}

open class ExternalOpenClass {
  init() {}
  open func openMethod() {}
  open var openProperty: Int = 0
  open subscript(index: MarkerForOpenSubscripts) -> Int {
    get { return 0 }
    set {}
  }

  public func nonOpenMethod() {}
  public var nonOpenProperty: Int = 0
  public subscript(index: MarkerForNonOpenSubscripts) -> Int {
    get { return 0 }
    set {}
  }

  public class PublicClass { public init() {} }
  open class OpenClass { public init() {} }
}

public struct ExternalStruct {
  open class OpenClass {
    public init() {}
  }
}

internal struct ExternalInternalStruct {
  open class OpenClass {
    public init() {}
  }
}