// Do not add an import of the Clang "Mixed" module here!

@objc class SwiftClass {
  init(x: Int) {}
  func pureSwiftMethod(x: Int?) -> Bool {
    return x ? true : false
  }
}

class PureSwiftClass {
  class func verify() -> Bool { return true }
}
