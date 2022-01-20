import Namespace

extension Namespace.Parent {
  public static func test() -> Int { 42 }
}

extension Namespace.Parent.Child {
  public static func test() -> Int { 52 }
}

extension Namespace.NestedNamespace.NestedStruct {
  public func test() -> Int { 62 }
}
