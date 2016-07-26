
@available(watchOS 2.0, *)
typealias CMMotionActivityHandler = (CMMotionActivity?) -> Void
@available(watchOS 2.0, *)
typealias CMMotionActivityQueryHandler = ([CMMotionActivity]?, NSError?) -> Void
@available(watchOS 2.0, *)
class CMMotionActivityManager : NSObject {
  @discardableResult
  class func isActivityAvailable() -> Bool
  func queryActivityStarting(from start: NSDate, to end: NSDate, to queue: NSOperationQueue, withHandler handler: CMMotionActivityQueryHandler)
  func startActivityUpdates(to queue: NSOperationQueue, withHandler handler: CMMotionActivityHandler)
  func stopActivityUpdates()
}
