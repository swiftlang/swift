import Foundation
import CoreGraphics

public class OC : NSObject {
  public let nsObject: NSObject = NSObject()
  public let nsString: NSString = ""
  public let cfString: CFString = "" as CFString
  public let aBlock: @convention(block) () -> () = {}
  public let ocnss: GenericOC<NSString> = GenericOC()
  public let occfs: GenericOC<CFString> = GenericOC()
}

public class GenericOC<T> : NSObject {
}

public class HasObjCClasses {
  let url = NSURL()
  let integer = NSInteger()
  let rect = CGRect(x: 0, y: 1, width: 2, height: 3)
}

@objc public protocol OP {}

public func closureHasObjCClasses(b: Bundle, c: NSCoding) -> () -> () {
  return { _ = b; _ = c }
}
