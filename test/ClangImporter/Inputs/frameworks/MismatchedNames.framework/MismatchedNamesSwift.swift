import Foundation
@_exported import MismatchedNames

@objc public class SwiftClass : NSObject {
  @objc public class func using(index: AnyObject) -> AnyObject? { return nil }
}
