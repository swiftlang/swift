
@available(iOS 6.0, *)
class EKReminder : EKCalendarItem {
  /*not inherited*/ init(eventStore eventStore: EKEventStore)
  @NSCopying var startDateComponents: NSDateComponents?
  @NSCopying var dueDateComponents: NSDateComponents?
  var isCompleted: Bool
  @NSCopying var completionDate: NSDate?
  var priority: Int
}
