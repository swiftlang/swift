
@available(OSX 10.5, *)
class NSOperation : NSObject {
  func start()
  func main()
  var isCancelled: Bool { get }
  func cancel()
  var isExecuting: Bool { get }
  var isFinished: Bool { get }
  var isConcurrent: Bool { get }
  @available(OSX 10.8, *)
  var isAsynchronous: Bool { get }
  var isReady: Bool { get }
  func addDependency(_ op: NSOperation)
  func removeDependency(_ op: NSOperation)
  var dependencies: [NSOperation] { get }
  var queuePriority: NSOperationQueuePriority
  @available(OSX 10.6, *)
  var completionBlock: (() -> Void)?
  @available(OSX 10.6, *)
  func waitUntilFinished()
  @available(OSX, introduced: 10.6, deprecated: 10.10)
  var threadPriority: Double
  @available(OSX 10.10, *)
  var qualityOfService: NSQualityOfService
  @available(OSX 10.10, *)
  var name: String?
}
enum NSOperationQueuePriority : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case veryLow
  case low
  case normal
  case high
  case veryHigh
}
@available(OSX 10.6, *)
class NSBlockOperation : NSOperation {
  convenience init(block block: () -> Void)
  func addExecutionBlock(_ block: () -> Void)
  var executionBlocks: [() -> Void] { get }
}
@available(OSX 10.5, *)
let NSInvocationOperationVoidResultException: String
@available(OSX 10.5, *)
let NSInvocationOperationCancelledException: String
let NSOperationQueueDefaultMaxConcurrentOperationCount: Int
@available(OSX 10.5, *)
class NSOperationQueue : NSObject {
  func addOperation(_ op: NSOperation)
  @available(OSX 10.6, *)
  func addOperations(_ ops: [NSOperation], waitUntilFinished wait: Bool)
  @available(OSX 10.6, *)
  func addOperation(_ block: () -> Void)
  var operations: [NSOperation] { get }
  @available(OSX 10.6, *)
  var operationCount: Int { get }
  var maxConcurrentOperationCount: Int
  var isSuspended: Bool
  @available(OSX 10.6, *)
  var name: String?
  @available(OSX 10.10, *)
  var qualityOfService: NSQualityOfService
  @available(OSX 10.10, *)
  unowned(unsafe) var underlyingQueue: @sil_unmanaged dispatch_queue_t?
  func cancelAllOperations()
  func waitUntilAllOperationsAreFinished()
  @available(OSX 10.6, *)
  @discardableResult
  class func current() -> NSOperationQueue?
  @available(OSX 10.6, *)
  @discardableResult
  class func main() -> NSOperationQueue
}
