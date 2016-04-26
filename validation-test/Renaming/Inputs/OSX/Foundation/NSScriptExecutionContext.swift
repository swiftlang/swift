
class NSScriptExecutionContext : NSObject {
  @discardableResult
  class func shared() -> NSScriptExecutionContext
  var topLevelObject: AnyObject?
  var objectBeingTested: AnyObject?
  var rangeContainerObject: AnyObject?
}
