
class NSThread : NSObject {
  @discardableResult
  class func current() -> NSThread
  class func detachNewThreadSelector(_ selector: Selector, toTarget target: AnyObject, with argument: AnyObject?)
  @discardableResult
  class func isMultiThreaded() -> Bool
  var threadDictionary: NSMutableDictionary { get }
  class func sleep(until date: NSDate)
  class func sleep(forTimeInterval ti: NSTimeInterval)
  class func exit()
  @discardableResult
  class func threadPriority() -> Double
  @discardableResult
  class func setThreadPriority(_ p: Double) -> Bool
  @available(tvOS 4.0, *)
  var threadPriority: Double
  @available(tvOS 8.0, *)
  var qualityOfService: NSQualityOfService
  @available(tvOS 2.0, *)
  @discardableResult
  class func callStackReturnAddresses() -> [NSNumber]
  @available(tvOS 4.0, *)
  @discardableResult
  class func callStackSymbols() -> [String]
  @available(tvOS 2.0, *)
  var name: String?
  @available(tvOS 2.0, *)
  var stackSize: Int
  @available(tvOS 2.0, *)
  var isMainThread: Bool { get }
  @available(tvOS 2.0, *)
  @discardableResult
  class func isMainThread() -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  class func main() -> NSThread
  @available(tvOS 2.0, *)
  convenience init(target target: AnyObject, selector selector: Selector, object argument: AnyObject?)
  @available(tvOS 2.0, *)
  var isExecuting: Bool { get }
  @available(tvOS 2.0, *)
  var isFinished: Bool { get }
  @available(tvOS 2.0, *)
  var isCancelled: Bool { get }
  @available(tvOS 2.0, *)
  func cancel()
  @available(tvOS 2.0, *)
  func start()
  @available(tvOS 2.0, *)
  func main()
}
let NSWillBecomeMultiThreadedNotification: String
let NSDidBecomeSingleThreadedNotification: String
let NSThreadWillExitNotification: String
extension NSObject {
  class func performSelector(onMainThread aSelector: Selector, with arg: AnyObject?, waitUntilDone wait: Bool, modes array: [String]?)
  func performSelector(onMainThread aSelector: Selector, with arg: AnyObject?, waitUntilDone wait: Bool, modes array: [String]?)
  class func performSelector(onMainThread aSelector: Selector, with arg: AnyObject?, waitUntilDone wait: Bool)
  func performSelector(onMainThread aSelector: Selector, with arg: AnyObject?, waitUntilDone wait: Bool)
  @available(tvOS 2.0, *)
  class func perform(_ aSelector: Selector, on thr: NSThread, with arg: AnyObject?, waitUntilDone wait: Bool, modes array: [String]?)
  @available(tvOS 2.0, *)
  func perform(_ aSelector: Selector, on thr: NSThread, with arg: AnyObject?, waitUntilDone wait: Bool, modes array: [String]?)
  @available(tvOS 2.0, *)
  class func perform(_ aSelector: Selector, on thr: NSThread, with arg: AnyObject?, waitUntilDone wait: Bool)
  @available(tvOS 2.0, *)
  func perform(_ aSelector: Selector, on thr: NSThread, with arg: AnyObject?, waitUntilDone wait: Bool)
  @available(tvOS 2.0, *)
  class func performSelector(inBackground aSelector: Selector, with arg: AnyObject?)
  @available(tvOS 2.0, *)
  func performSelector(inBackground aSelector: Selector, with arg: AnyObject?)
}
