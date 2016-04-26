
@available(watchOS 2.0, *)
class UILocalNotification : NSObject, NSCopying, NSCoding {
  @NSCopying var fireDate: NSDate?
  @NSCopying var timeZone: NSTimeZone?
  var repeatInterval: NSCalendarUnit
  @NSCopying var repeatCalendar: NSCalendar?
  @available(watchOS 2.0, *)
  var regionTriggersOnce: Bool
  var alertBody: String?
  var hasAction: Bool
  var alertAction: String?
  var alertLaunchImage: String?
  @available(watchOS 2.0, *)
  var alertTitle: String?
  var soundName: String?
  var applicationIconBadgeNumber: Int
  var userInfo: [NSObject : AnyObject]?
  @available(watchOS 2.0, *)
  var category: String?
}
@available(watchOS 2.0, *)
let UILocalNotificationDefaultSoundName: String
