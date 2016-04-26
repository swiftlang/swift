
@available(iOS 8.0, *)
class HMTimerTrigger : HMTrigger {
  init(name name: String, fireDate fireDate: NSDate, timeZone timeZone: NSTimeZone?, recurrence recurrence: NSDateComponents?, recurrenceCalendar recurrenceCalendar: NSCalendar?)
  @NSCopying var fireDate: NSDate { get }
  @NSCopying var timeZone: NSTimeZone? { get }
  @NSCopying var recurrence: NSDateComponents? { get }
  @NSCopying var recurrenceCalendar: NSCalendar? { get }
  func updateFireDate(_ fireDate: NSDate, completionHandler completion: (NSError?) -> Void)
  func updateTimeZone(_ timeZone: NSTimeZone?, completionHandler completion: (NSError?) -> Void)
  func updateRecurrence(_ recurrence: NSDateComponents?, completionHandler completion: (NSError?) -> Void)
}
