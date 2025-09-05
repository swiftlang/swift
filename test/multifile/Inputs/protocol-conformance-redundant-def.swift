public protocol Hello {
  func hello()
}

open class Super {
  public init() {}
}

open class GenericSuperClass<T> {
  public init() {}
}

public struct GenericStruct<T> {
  public init() {}
}