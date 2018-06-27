// Import this class in the test.
@objc public class Bar_ImportedObjcClass {
  @objc public func bar_ImportedObjcClass_InstanceFunc1() {}
  @objc public class func bar_ImportedObjcClass_ClassFunc1() {}
  @objc public subscript(i: Bar_ImportedObjcClass) -> Int {
    get {
      return 0
    }
  }
  @objc public var bar_ImportedObjcClass_Property1: Int = 0
}

// Don't import this class in the test.
@objc public class Bar_NonImportedObjcClass {
  public func ERROR() {}
}

