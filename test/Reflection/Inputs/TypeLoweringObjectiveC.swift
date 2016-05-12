import Foundation

public struct FunctionStruct {
  public let blockFunction: @convention(block) () -> ()
}

public class HasObjCClasses {
  let url = NSURL()
  let integer = NSInteger()
}

public class NSObjectSubclass : NSObject {}

