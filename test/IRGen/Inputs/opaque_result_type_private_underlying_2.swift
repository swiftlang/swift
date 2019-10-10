public protocol A {
  associatedtype Assoc
  func bindAssoc() -> Assoc
}

public protocol Q {}

fileprivate struct PrivateS : Q {
  init() {}
}

public struct G : A {
  public init() {}
   public func bindAssoc() -> some Q {
     return PrivateS()
   }
}
