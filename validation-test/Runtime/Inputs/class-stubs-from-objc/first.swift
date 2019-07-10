import Foundation

open class BaseClass : NSObject {
  @objc dynamic open func instanceMethod() -> Int {
    return 42
  }

  @objc dynamic open class func classMethod() -> Int {
    return 31337
  }
}
