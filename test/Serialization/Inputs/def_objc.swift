@class_protocol @objc protocol ObjCProto {
  func doSomething()
}

@objc class ObjCClass {
  class func classMethod() {}
  func implicitlyObjC() {}

  @IBOutlet var outlet : ObjCClass = ObjCClass()
  @IBAction func performAction(_: AnyObject?) {}
}

class NonObjCClass : ObjCProto {
  func doSomething() {}

  @objc func objcMethod() {}
  @objc var objcProp : ObjCClass = ObjCClass()

  @objc subscript (i : Int) -> Int {
    return 5
  }

  @IBOutlet var outlet : ObjCClass = ObjCClass()
  @IBAction func performAction(_: AnyObject?) {}
}
