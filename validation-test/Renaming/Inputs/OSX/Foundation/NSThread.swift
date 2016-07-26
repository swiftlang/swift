
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
  @available(OSX 10.6, *)
  var threadPriority: Double
  @available(OSX 10.10, *)
  var qualityOfService: NSQualityOfService
  @available(OSX 10.5, *)
  @discardableResult
  class func callStackReturnAddresses() -> [NSNumber]
  @available(OSX 10.6, *)
  @discardableResult
  class func callStackSymbols() -> [String]
  @available(OSX 10.5, *)
  var name: String?
  @available(OSX 10.5, *)
  var stackSize: Int
  @available(OSX 10.5, *)
  var isMainThread: Bool { get }
  @available(OSX 10.5, *)
  @discardableResult
  class func isMainThread() -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  class func main() -> NSThread
  @available(OSX 10.5, *)
  convenience init(target target: AnyObject, selector selector: Selector, object argument: AnyObject?)
  @available(OSX 10.5, *)
  var isExecuting: Bool { get }
  @available(OSX 10.5, *)
  var isFinished: Bool { get }
  @available(OSX 10.5, *)
  var isCancelled: Bool { get }
  @available(OSX 10.5, *)
  func cancel()
  @available(OSX 10.5, *)
  func start()
  @available(OSX 10.5, *)
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
  @available(OSX 10.5, *)
  class func perform(_ aSelector: Selector, on thr: NSThread, with arg: AnyObject?, waitUntilDone wait: Bool, modes array: [String]?)
  @available(OSX 10.5, *)
  func perform(_ aSelector: Selector, on thr: NSThread, with arg: AnyObject?, waitUntilDone wait: Bool, modes array: [String]?)
  @available(OSX 10.5, *)
  class func perform(_ aSelector: Selector, on thr: NSThread, with arg: AnyObject?, waitUntilDone wait: Bool)
  @available(OSX 10.5, *)
  func perform(_ aSelector: Selector, on thr: NSThread, with arg: AnyObject?, waitUntilDone wait: Bool)
  @available(OSX 10.5, *)
  class func performSelector(inBackground aSelector: Selector, with arg: AnyObject?)
  @available(OSX 10.5, *)
  func performSelector(inBackground aSelector: Selector, with arg: AnyObject?)
}
