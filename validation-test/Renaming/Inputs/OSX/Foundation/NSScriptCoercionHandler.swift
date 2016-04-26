
class NSScriptCoercionHandler : NSObject {
  @discardableResult
  class func shared() -> NSScriptCoercionHandler
  @discardableResult
  func coerceValue(_ value: AnyObject, to toClass: AnyClass) -> AnyObject?
  func registerCoercer(_ coercer: AnyObject, selector selector: Selector, toConvertFrom fromClass: AnyClass, to toClass: AnyClass)
}
