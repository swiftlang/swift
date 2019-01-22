import Foundation

public struct FunctionStruct {
  public let blockFunction: @convention(block) () -> ()
}

public class HasObjCClasses {
  let url = NSURL()
  let integer = NSInteger()
}

public class NSObjectSubclass : NSObject {}

@objc public enum ObjCEnum : Int {
  case first
  case second
}

public struct HasObjCEnum {
  let optionalEnum: ObjCEnum?
  let reference: AnyObject
}

public struct UnownedReferenceStruct {
  unowned var unownedRef: NSObjectSubclass
}
