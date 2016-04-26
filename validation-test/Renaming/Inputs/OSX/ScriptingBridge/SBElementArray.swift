
@available(OSX 10.5, *)
class SBElementArray : NSMutableArray {
  @discardableResult
  func object(withName name: String) -> AnyObject
  @discardableResult
  func object(withID identifier: AnyObject) -> AnyObject
  @discardableResult
  func object(atLocation location: AnyObject) -> AnyObject
  @discardableResult
  func array(byApplying selector: Selector) -> [AnyObject]
  @discardableResult
  func array(byApplying aSelector: Selector, with argument: AnyObject) -> [AnyObject]
  @discardableResult
  func get() -> [AnyObject]?
}
