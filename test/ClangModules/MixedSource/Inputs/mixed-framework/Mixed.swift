// Do not add an import of the Clang "Mixed" module here!

@objc public class SwiftClass {
  public init(x: Int) {}
  public func pureSwiftMethod(x: Int?) -> Bool {
    return x ? true : false
  }
}

public class PureSwiftClass {
  public class func verify() -> Bool { return true }
}
