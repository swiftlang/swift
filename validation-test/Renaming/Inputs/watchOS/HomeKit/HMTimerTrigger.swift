
@available(watchOS 20000, *)
class HMTimerTrigger : HMTrigger {
  @NSCopying var fireDate: NSDate { get }
  @NSCopying var timeZone: NSTimeZone? { get }
  @NSCopying var recurrence: NSDateComponents? { get }
  @NSCopying var recurrenceCalendar: NSCalendar? { get }
}
