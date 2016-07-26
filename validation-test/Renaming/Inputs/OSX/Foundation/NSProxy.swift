
class NSProxy : NSObjectProtocol {
  @discardableResult
  class func alloc() -> Self
  @discardableResult
  class func `class`() -> AnyClass
  class func forwardInvocation(_ invocation: NSInvocation)
  func forwardInvocation(_ invocation: NSInvocation)
  class func dealloc()
  func dealloc()
  class func finalize()
  func finalize()
  @discardableResult
  class func responds(to aSelector: Selector) -> Bool
}
