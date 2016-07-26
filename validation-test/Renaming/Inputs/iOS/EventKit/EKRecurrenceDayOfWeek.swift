
@available(iOS 4.0, *)
class EKRecurrenceDayOfWeek : NSObject, NSCopying {
  convenience init(_ dayOfTheWeek: EKWeekday)
  convenience init(_ dayOfTheWeek: EKWeekday, weekNumber weekNumber: Int)
  init(dayOfTheWeek dayOfTheWeek: EKWeekday, weekNumber weekNumber: Int)
  var dayOfTheWeek: EKWeekday { get }
  var weekNumber: Int { get }
}
