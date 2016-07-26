
typealias CMStepQueryHandler = (Int, NSError?) -> Void
typealias CMStepUpdateHandler = (Int, NSDate, NSError?) -> Void
@available(iOS, introduced: 7.0, deprecated: 8.0, message: "Use CMPedometer instead")
class CMStepCounter : NSObject {
  @discardableResult
  class func isStepCountingAvailable() -> Bool
  func queryStepCountStarting(from start: NSDate, to end: NSDate, to queue: NSOperationQueue, withHandler handler: CMStepQueryHandler)
  func startStepCountingUpdates(to queue: NSOperationQueue, updateOn stepCounts: Int, withHandler handler: CMStepUpdateHandler)
  func stopStepCountingUpdates()
}
