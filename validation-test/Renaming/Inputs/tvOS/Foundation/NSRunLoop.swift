
let NSDefaultRunLoopMode: String
@available(tvOS 2.0, *)
let NSRunLoopCommonModes: String
class NSRunLoop : NSObject {
  @discardableResult
  class func current() -> NSRunLoop
  @available(tvOS 2.0, *)
  @discardableResult
  class func main() -> NSRunLoop
  var currentMode: String? { get }
  @discardableResult
  func getCFRunLoop() -> CFRunLoop
  func add(_ timer: NSTimer, forMode mode: String)
  func add(_ aPort: NSPort, forMode mode: String)
  func remove(_ aPort: NSPort, forMode mode: String)
  @discardableResult
  func limitDate(forMode mode: String) -> NSDate?
  func acceptInput(forMode mode: String, before limitDate: NSDate)
}
extension NSRunLoop {
  func run()
  func run(until limitDate: NSDate)
  @discardableResult
  func run(mode mode: String, before limitDate: NSDate) -> Bool
}
extension NSObject {
  class func perform(_ aSelector: Selector, with anArgument: AnyObject?, afterDelay delay: NSTimeInterval, inModes modes: [String])
  func perform(_ aSelector: Selector, with anArgument: AnyObject?, afterDelay delay: NSTimeInterval, inModes modes: [String])
  class func perform(_ aSelector: Selector, with anArgument: AnyObject?, afterDelay delay: NSTimeInterval)
  func perform(_ aSelector: Selector, with anArgument: AnyObject?, afterDelay delay: NSTimeInterval)
  class func cancelPreviousPerformRequests(withTarget aTarget: AnyObject, selector aSelector: Selector, object anArgument: AnyObject?)
  class func cancelPreviousPerformRequests(withTarget aTarget: AnyObject)
}
extension NSRunLoop {
  func perform(_ aSelector: Selector, target target: AnyObject, argument arg: AnyObject?, order order: Int, modes modes: [String])
  func cancelPerform(_ aSelector: Selector, target target: AnyObject, argument arg: AnyObject?)
  func cancelPerformSelectors(withTarget target: AnyObject)
}
