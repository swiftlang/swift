// Do not add an import of the Clang "Mixed" module here!

@objc public class SwiftClass {
  public init(x: Int) {}
  public func pureSwiftMethod(_ x: Int?) -> Bool {
    return x != nil ? true : false
  }

  @objc public func method() {}
  @objc public var integerProperty: Int = 0
}

extension SwiftClass {
  @objc public func extensionMethod() {}
}

public class PureSwiftClass {
  public class func verify() -> Bool { return true }
}

@objc(SwiftProtoWithCustomName)
public protocol CustomName {}

@objc(SwiftClassWithCustomName)
public class CustomNameClass : CustomName {
  public init() {}
  @nonobjc func pureSwiftMethod() {}
}

@objc public protocol SwiftProto {
  @objc func protoMethod()
  @objc var protoProperty: Int { get }
}
