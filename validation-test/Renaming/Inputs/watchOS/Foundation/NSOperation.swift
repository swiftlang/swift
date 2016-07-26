
@available(watchOS 2.0, *)
class NSOperation : NSObject {
  func start()
  func main()
  var isCancelled: Bool { get }
  func cancel()
  var isExecuting: Bool { get }
  var isFinished: Bool { get }
  var isConcurrent: Bool { get }
  @available(watchOS 2.0, *)
  var isAsynchronous: Bool { get }
  var isReady: Bool { get }
  func addDependency(_ op: NSOperation)
  func removeDependency(_ op: NSOperation)
  var dependencies: [NSOperation] { get }
  var queuePriority: NSOperationQueuePriority
  @available(watchOS 2.0, *)
  var completionBlock: (() -> Void)?
  @available(watchOS 2.0, *)
  func waitUntilFinished()
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  var threadPriority: Double
  @available(watchOS 2.0, *)
  var qualityOfService: NSQualityOfService
  @available(watchOS 2.0, *)
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
@available(watchOS 2.0, *)
class NSBlockOperation : NSOperation {
  convenience init(block block: () -> Void)
  func addExecutionBlock(_ block: () -> Void)
  var executionBlocks: [() -> Void] { get }
}
@available(watchOS 2.0, *)
let NSInvocationOperationVoidResultException: String
@available(watchOS 2.0, *)
let NSInvocationOperationCancelledException: String
let NSOperationQueueDefaultMaxConcurrentOperationCount: Int
@available(watchOS 2.0, *)
class NSOperationQueue : NSObject {
  func addOperation(_ op: NSOperation)
  @available(watchOS 2.0, *)
  func addOperations(_ ops: [NSOperation], waitUntilFinished wait: Bool)
  @available(watchOS 2.0, *)
  func addOperation(_ block: () -> Void)
  var operations: [NSOperation] { get }
  @available(watchOS 2.0, *)
  var operationCount: Int { get }
  var maxConcurrentOperationCount: Int
  var isSuspended: Bool
  @available(watchOS 2.0, *)
  var name: String?
  @available(watchOS 2.0, *)
  var qualityOfService: NSQualityOfService
  @available(watchOS 2.0, *)
  unowned(unsafe) var underlyingQueue: @sil_unmanaged dispatch_queue_t?
  func cancelAllOperations()
  func waitUntilAllOperationsAreFinished()
  @available(watchOS 2.0, *)
  @discardableResult
  class func current() -> NSOperationQueue?
  @available(watchOS 2.0, *)
  @discardableResult
  class func main() -> NSOperationQueue
}
