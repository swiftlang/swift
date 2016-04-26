
enum UIDatePickerMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case time
  case date
  case dateAndTime
  case countDownTimer
}
@available(iOS 2.0, *)
class UIDatePicker : UIControl, NSCoding {
  var datePickerMode: UIDatePickerMode
  var locale: NSLocale?
  @NSCopying var calendar: NSCalendar!
  var timeZone: NSTimeZone?
  var date: NSDate
  var minimumDate: NSDate?
  var maximumDate: NSDate?
  var countDownDuration: NSTimeInterval
  var minuteInterval: Int
  func setDate(_ date: NSDate, animated animated: Bool)
}
