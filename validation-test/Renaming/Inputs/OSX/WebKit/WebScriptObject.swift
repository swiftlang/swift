
extension NSObject {
  @available(OSX 10.4, *)
  @discardableResult
  class func webScriptName(for selector: Selector!) -> String!
  @available(OSX 10.4, *)
  @discardableResult
  class func isSelectorExcluded(fromWebScript selector: Selector!) -> Bool
  @available(OSX 10.4, *)
  @discardableResult
  class func webScriptName(forKey name: UnsafePointer<Int8>!) -> String!
  @available(OSX 10.4, *)
  @discardableResult
  class func isKeyExcluded(fromWebScript name: UnsafePointer<Int8>!) -> Bool
  @available(OSX 10.4, *)
  @discardableResult
  class func invokeUndefinedMethod(fromWebScript name: String!, withArguments arguments: [AnyObject]!) -> AnyObject!
  @available(OSX 10.4, *)
  @discardableResult
  func invokeUndefinedMethod(fromWebScript name: String!, withArguments arguments: [AnyObject]!) -> AnyObject!
  @available(OSX 10.4, *)
  @discardableResult
  class func invokeDefaultMethod(withArguments arguments: [AnyObject]!) -> AnyObject!
  @available(OSX 10.4, *)
  @discardableResult
  func invokeDefaultMethod(withArguments arguments: [AnyObject]!) -> AnyObject!
  @available(OSX 10.4, *)
  class func finalizeForWebScript()
  @available(OSX 10.4, *)
  func finalizeForWebScript()
}
@available(OSX 10.4, *)
class WebScriptObject : NSObject {
  @discardableResult
  class func throwException(_ exceptionMessage: String!) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func jsObject() -> JSObjectRef!
  @discardableResult
  func callWebScriptMethod(_ name: String!, withArguments arguments: [AnyObject]!) -> AnyObject!
  @discardableResult
  func evaluateWebScript(_ script: String!) -> AnyObject!
  func removeWebScriptKey(_ name: String!)
  @discardableResult
  func stringRepresentation() -> String!
  @discardableResult
  func webScriptValue(at index: UInt32) -> AnyObject!
  func setWebScriptValueAt(_ index: UInt32, value value: AnyObject!)
  func setException(_ description: String!)
  @discardableResult
  func jsValue() -> JSValue!
}
@available(OSX 10.4, *)
class WebUndefined : NSObject, NSCoding, NSCopying {
}
