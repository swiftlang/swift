public protocol MyProtocol {
  static func method()
}

public func take<T: MyProtocol>(_: T) { T.method() }

@available(macOS 10.15, *)
@_originallyDefinedIn(module: "Lib", macOS 13)
public struct MyStruct: MyProtocol {
  public init() {}
  public static func method() {}
}
