
@available(tvOS 7.0, *)
class JSContext : NSObject {
  init!(virtualMachine virtualMachine: JSVirtualMachine!)
  @discardableResult
  func evaluateScript(_ script: String!) -> JSValue!
  @available(tvOS 8.0, *)
  @discardableResult
  func evaluateScript(_ script: String!, withSourceURL sourceURL: NSURL!) -> JSValue!
  @discardableResult
  class func current() -> JSContext!
  @available(tvOS 8.0, *)
  @discardableResult
  class func currentCallee() -> JSValue!
  @discardableResult
  class func currentThis() -> JSValue!
  @discardableResult
  class func currentArguments() -> [AnyObject]!
  var globalObject: JSValue! { get }
  var exception: JSValue!
  var exceptionHandler: ((JSContext!, JSValue!) -> Void)!
  var virtualMachine: JSVirtualMachine! { get }
  @available(tvOS 8.0, *)
  var name: String!
}
extension JSContext {
  @discardableResult
  func objectForKeyedSubscript(_ key: AnyObject!) -> JSValue!
  func setObject(_ object: AnyObject!, forKeyedSubscript key: protocol<NSCopying, NSObjectProtocol>!)
}
extension JSContext {
  /*not inherited*/ init!(jsGlobalContextRef jsGlobalContextRef: JSGlobalContextRef!)
  var jsGlobalContextRef: JSGlobalContextRef! { get }
}
