
protocol NSCopying {
  @discardableResult
  func copy(with zone: NSZone? = nil) -> AnyObject
}
protocol NSMutableCopying {
  @discardableResult
  func mutableCopy(with zone: NSZone? = nil) -> AnyObject
}
protocol NSCoding {
  func encode(with aCoder: NSCoder)
  init?(coder aDecoder: NSCoder)
}
protocol NSSecureCoding : NSCoding {
  @discardableResult
  static func supportsSecureCoding() -> Bool
}
extension NSObject {
  @discardableResult
  class func version() -> Int
  class func setVersion(_ aVersion: Int)
  var classForCoder: AnyClass { get }
  @discardableResult
  class func replacementObject(for aCoder: NSCoder) -> AnyObject?
  @discardableResult
  func replacementObject(for aCoder: NSCoder) -> AnyObject?
  @discardableResult
  class func awakeAfter(using aDecoder: NSCoder) -> AnyObject?
  @discardableResult
  func awakeAfter(using aDecoder: NSCoder) -> AnyObject?
  class func classForCoder() -> AnyClass
}
extension NSObject {
}
protocol NSDiscardableContent {
  @discardableResult
  func beginContentAccess() -> Bool
  func endContentAccess()
  func discardContentIfPossible()
  @discardableResult
  func isContentDiscarded() -> Bool
}
extension NSObject {
  @available(OSX 10.6, *)
  var autoContentAccessingProxy: AnyObject { get }
  class func autoContentAccessingProxy() -> AnyObject
}
@discardableResult
func CFBridgingRetain(_ X: AnyObject?) -> CFTypeRef?
