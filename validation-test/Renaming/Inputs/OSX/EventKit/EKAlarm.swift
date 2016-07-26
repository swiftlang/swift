
@available(OSX 10.8, *)
class EKAlarm : EKObject, NSCopying {
  /*not inherited*/ init(absoluteDate date: NSDate)
  /*not inherited*/ init(relativeOffset offset: NSTimeInterval)
  var relativeOffset: NSTimeInterval
  @NSCopying var absoluteDate: NSDate?
  @NSCopying var structuredLocation: EKStructuredLocation?
  var proximity: EKAlarmProximity
  @available(OSX 10.8, *)
  var type: EKAlarmType { get }
  @available(OSX 10.8, *)
  var emailAddress: String?
  @available(OSX 10.8, *)
  var soundName: String?
}
