public struct External<A> {
  public var property: A
  public var intProperty: Int
  public subscript<B: Hashable>(index: B) -> A { return property }

  public private(set) var privateSetProperty: Int
  public private(set) subscript(privateSet index: Int) -> Int {
    get { return index }
    set { }
  }
}

public struct ExternalEmptySubscript {
  public subscript() -> Int { return 0 }
}

public protocol ExternalProto {
  var protoReqt: Int { get set }
}
