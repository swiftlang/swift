
class NSDateFormatter : NSFormatter {
  @available(OSX 10.10, *)
  var formattingContext: NSFormattingContext
  func getObjectValue(_ obj: AutoreleasingUnsafeMutablePointer<AnyObject?>?, for string: String, range rangep: UnsafeMutablePointer<NSRange>?) throws
  @discardableResult
  func string(from date: NSDate) -> String
  @discardableResult
  func date(from string: String) -> NSDate?
  @available(OSX 10.6, *)
  @discardableResult
  class func localizedString(from date: NSDate, dateStyle dstyle: NSDateFormatterStyle, timeStyle tstyle: NSDateFormatterStyle) -> String
  @available(OSX 10.6, *)
  @discardableResult
  class func dateFormat(fromTemplate tmplate: String, options opts: Int, locale locale: NSLocale?) -> String?
  @discardableResult
  class func defaultFormatterBehavior() -> NSDateFormatterBehavior
  class func setDefaultFormatterBehavior(_ behavior: NSDateFormatterBehavior)
  @available(OSX 10.10, *)
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
  @available(OSX 10.5, *)
  var longEraSymbols: [String]!
  @available(OSX 10.5, *)
  var veryShortMonthSymbols: [String]!
  @available(OSX 10.5, *)
  var standaloneMonthSymbols: [String]!
  @available(OSX 10.5, *)
  var shortStandaloneMonthSymbols: [String]!
  @available(OSX 10.5, *)
  var veryShortStandaloneMonthSymbols: [String]!
  @available(OSX 10.5, *)
  var veryShortWeekdaySymbols: [String]!
  @available(OSX 10.5, *)
  var standaloneWeekdaySymbols: [String]!
  @available(OSX 10.5, *)
  var shortStandaloneWeekdaySymbols: [String]!
  @available(OSX 10.5, *)
  var veryShortStandaloneWeekdaySymbols: [String]!
  @available(OSX 10.5, *)
  var quarterSymbols: [String]!
  @available(OSX 10.5, *)
  var shortQuarterSymbols: [String]!
  @available(OSX 10.5, *)
  var standaloneQuarterSymbols: [String]!
  @available(OSX 10.5, *)
  var shortStandaloneQuarterSymbols: [String]!
  @available(OSX 10.5, *)
  @NSCopying var gregorianStartDate: NSDate?
  @available(OSX 10.6, *)
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
  case behavior10_0
  case behavior10_4
}
extension NSDateFormatter {
}
