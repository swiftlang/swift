@objc public protocol ObjCProto {
  func doSomething()
}

@objc @objcMembers public class ObjCClass {
  public dynamic class func classMethod() {}
  public dynamic func implicitlyObjC() {}

  @IBOutlet public var outlet : ObjCClass! = ObjCClass()
  @IBAction public func performAction(_: AnyObject?) {}
}

@objcMembers public class NonObjCClass : ObjCProto {
  public dynamic func doSomething() {}

  dynamic public func objcMethod() {}
  dynamic public var objcProp : ObjCClass = ObjCClass()

  dynamic public subscript (i : Int) -> Int {
    return 5
  }

  @IBOutlet public var outlet : ObjCClass! = ObjCClass()
  @IBAction public func performAction(_: AnyObject?) {}
}
