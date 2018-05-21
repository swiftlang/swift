public protocol SimpleProto {
  func successor() -> Self
}

public protocol ComplexProto : SimpleProto {
  func predecessor() -> Self
}

public protocol ProtoUser {
  associatedtype Element
  associatedtype Impl: SimpleProto
  var start: Impl { get }
  var end: Impl { get }
  subscript(_: Impl) -> Element { get }
}

private protocol PrivateProto {
  func foo()
}

open class ConformsToPrivateProto : PrivateProto {
  func foo() {}
}
