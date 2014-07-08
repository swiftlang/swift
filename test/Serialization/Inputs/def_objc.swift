@class_protocol @objc public protocol ObjCProto {
  func doSomething()
}

@objc public class ObjCClass {
  public class func classMethod() {}
  public func implicitlyObjC() {}

  @IBOutlet public var outlet : ObjCClass = ObjCClass()
  @IBAction public func performAction(_: AnyObject?) {}
}

public class NonObjCClass : ObjCProto {
  public func doSomething() {}

  @objc public func objcMethod() {}
  @objc public var objcProp : ObjCClass = ObjCClass()

  @objc public subscript (i : Int) -> Int {
    return 5
  }

  @IBOutlet public var outlet : ObjCClass! = ObjCClass()
  @IBAction public func performAction(_: AnyObject?) {}
}
