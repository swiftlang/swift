// Do not add an import of the Clang "Mixed" module here!

@objc public class SwiftClass {
  public init(x: Int) {}
  public func pureSwiftMethod(x: Int?) -> Bool {
    return x != nil ? true : false
  }
}

public class PureSwiftClass {
  public class func verify() -> Bool { return true }
}

@objc(SwiftProtoWithCustomName)
public protocol CustomNameType {}

@objc(SwiftClassWithCustomName)
public class CustomNameClass : CustomNameType {
  public init() {}
  @nonobjc func pureSwiftMethod() {}
}
