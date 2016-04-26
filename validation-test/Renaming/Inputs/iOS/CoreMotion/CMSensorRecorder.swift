
@available(iOS 9.0, *)
class CMRecordedAccelerometerData : CMAccelerometerData {
  var identifier: UInt64 { get }
  var startDate: NSDate { get }
}
@available(iOS 9.0, *)
class CMSensorDataList : NSObject, NSFastEnumeration {
}
@available(iOS 9.0, *)
class CMSensorRecorder : NSObject {
  @discardableResult
  class func isAccelerometerRecordingAvailable() -> Bool
  @discardableResult
  class func isAuthorizedForRecording() -> Bool
  @discardableResult
  func accelerometerData(from fromDate: NSDate, to toDate: NSDate) -> CMSensorDataList?
  func recordAccelerometer(forDuration duration: NSTimeInterval)
}
