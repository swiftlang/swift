
@available(OSX 10.10, *)
enum NSDateIntervalFormatterStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case noStyle
  case shortStyle
  case mediumStyle
  case longStyle
  case fullStyle
}
@available(OSX 10.10, *)
class NSDateIntervalFormatter : NSFormatter {
  @NSCopying var locale: NSLocale!
  @NSCopying var calendar: NSCalendar!
  @NSCopying var timeZone: NSTimeZone!
  var dateTemplate: String!
  var dateStyle: NSDateIntervalFormatterStyle
  var timeStyle: NSDateIntervalFormatterStyle
  @discardableResult
  func string(from fromDate: NSDate, to toDate: NSDate) -> String
}
