public struct CommonStruct { }

public protocol CommonP1 {
  associatedtype AssocType
}

public struct CommonRequiresP1<T: CommonP1> {
  public init() { }
}
