
@available(iOS 8.0, *)
class CMPedometerData : NSObject, NSSecureCoding, NSCopying {
  var startDate: NSDate { get }
  var endDate: NSDate { get }
  var numberOfSteps: NSNumber { get }
  var distance: NSNumber? { get }
  var floorsAscended: NSNumber? { get }
  var floorsDescended: NSNumber? { get }
  @available(iOS 9.0, *)
  var currentPace: NSNumber? { get }
  @available(iOS 9.0, *)
  var currentCadence: NSNumber? { get }
}
typealias CMPedometerHandler = (CMPedometerData?, NSError?) -> Void
@available(iOS 8.0, *)
class CMPedometer : NSObject {
  @discardableResult
  class func isStepCountingAvailable() -> Bool
  @discardableResult
  class func isDistanceAvailable() -> Bool
  @discardableResult
  class func isFloorCountingAvailable() -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  class func isPaceAvailable() -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  class func isCadenceAvailable() -> Bool
  func queryPedometerData(from start: NSDate, to end: NSDate, withHandler handler: CMPedometerHandler)
  func startUpdates(from start: NSDate, withHandler handler: CMPedometerHandler)
  func stopUpdates()
}
