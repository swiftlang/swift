
class NSDateFormatter : NSFormatter {
  @available(watchOS 2.0, *)
  var formattingContext: NSFormattingContext
  func getObjectValue(_ obj: AutoreleasingUnsafeMutablePointer<AnyObject?>?, for string: String, range rangep: UnsafeMutablePointer<NSRange>?) throws
  @discardableResult
  func string(from date: NSDate) -> String
  @discardableResult
  func date(from string: String) -> NSDate?
  @available(watchOS 2.0, *)
  @discardableResult
  class func localizedString(from date: NSDate, dateStyle dstyle: NSDateFormatterStyle, timeStyle tstyle: NSDateFormatterStyle) -> String
  @available(watchOS 2.0, *)
  @discardableResult
  class func dateFormat(fromTemplate tmplate: String, options opts: Int, locale locale: NSLocale?) -> String?
  @discardableResult
  class func defaultFormatterBehavior() -> NSDateFormatterBehavior
  class func setDefaultFormatterBehavior(_ behavior: NSDateFormatterBehavior)
  @available(watchOS 2.0, *)
  func setLocalizedDateFormatFromTemplate(_ dateFormatTemplate: String)
  var dateFormat: String!
  var dateStyle: NSDateFormatterStyle
  var timeStyle: NSDateFormatterStyle
  @NSCopying var locale: NSLocale!
  var generatesCalendarDates: Bool
  var formatterBehavior: NSDateFormatterBehavior
  @NSCopying var timeZone: NSTimeZone!
  @NSCopying var calendar: NSCalendar!
  var isLenient: Bool
  @NSCopying var twoDigitStartDate: NSDate?
  @NSCopying var defaultDate: NSDate?
  var eraSymbols: [String]!
  var monthSymbols: [String]!
  var shortMonthSymbols: [String]!
  var weekdaySymbols: [String]!
  var shortWeekdaySymbols: [String]!
  var amSymbol: String!
  var pmSymbol: String!
  @available(watchOS 2.0, *)
  var longEraSymbols: [String]!
  @available(watchOS 2.0, *)
  var veryShortMonthSymbols: [String]!
  @available(watchOS 2.0, *)
  var standaloneMonthSymbols: [String]!
  @available(watchOS 2.0, *)
  var shortStandaloneMonthSymbols: [String]!
  @available(watchOS 2.0, *)
  var veryShortStandaloneMonthSymbols: [String]!
  @available(watchOS 2.0, *)
  var veryShortWeekdaySymbols: [String]!
  @available(watchOS 2.0, *)
  var standaloneWeekdaySymbols: [String]!
  @available(watchOS 2.0, *)
  var shortStandaloneWeekdaySymbols: [String]!
  @available(watchOS 2.0, *)
  var veryShortStandaloneWeekdaySymbols: [String]!
  @available(watchOS 2.0, *)
  var quarterSymbols: [String]!
  @available(watchOS 2.0, *)
  var shortQuarterSymbols: [String]!
  @available(watchOS 2.0, *)
  var standaloneQuarterSymbols: [String]!
  @available(watchOS 2.0, *)
  var shortStandaloneQuarterSymbols: [String]!
  @available(watchOS 2.0, *)
  @NSCopying var gregorianStartDate: NSDate?
  @available(watchOS 2.0, *)
  var doesRelativeDateFormatting: Bool
}
enum NSDateFormatterStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case noStyle
  case shortStyle
  case mediumStyle
  case longStyle
  case fullStyle
}
enum NSDateFormatterBehavior : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case behaviorDefault
  case behavior10_4
}
