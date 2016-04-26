
@available(OSX 10.5, *)
class SBObject : NSObject, NSCoding {
  init(properties properties: [NSObject : AnyObject])
  init(data data: AnyObject)
  @discardableResult
  func get() -> AnyObject?
  @available(OSX 10.6, *)
  @discardableResult
  func lastError() -> NSError?
}
extension SBObject {
  init(elementCode code: DescType, properties properties: [String : AnyObject]?, data data: AnyObject?)
  @discardableResult
  func property(withCode code: AEKeyword) -> SBObject
  @discardableResult
  func property(with cls: AnyClass, code code: AEKeyword) -> SBObject
  @discardableResult
  func elementArray(withCode code: DescType) -> SBElementArray
  func setTo(_ value: AnyObject?)
}
