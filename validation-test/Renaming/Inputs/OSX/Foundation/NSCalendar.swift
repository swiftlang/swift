
@available(OSX 10.6, *)
let NSCalendarIdentifierGregorian: String
@available(OSX 10.6, *)
let NSCalendarIdentifierBuddhist: String
@available(OSX 10.6, *)
let NSCalendarIdentifierChinese: String
@available(OSX 10.6, *)
let NSCalendarIdentifierCoptic: String
@available(OSX 10.6, *)
let NSCalendarIdentifierEthiopicAmeteMihret: String
@available(OSX 10.6, *)
let NSCalendarIdentifierEthiopicAmeteAlem: String
@available(OSX 10.6, *)
let NSCalendarIdentifierHebrew: String
@available(OSX 10.6, *)
let NSCalendarIdentifierISO8601: String
@available(OSX 10.6, *)
let NSCalendarIdentifierIndian: String
@available(OSX 10.6, *)
let NSCalendarIdentifierIslamic: String
@available(OSX 10.6, *)
let NSCalendarIdentifierIslamicCivil: String
@available(OSX 10.6, *)
let NSCalendarIdentifierJapanese: String
@available(OSX 10.6, *)
let NSCalendarIdentifierPersian: String
@available(OSX 10.6, *)
let NSCalendarIdentifierRepublicOfChina: String
@available(OSX 10.10, *)
let NSCalendarIdentifierIslamicTabular: String
@available(OSX 10.10, *)
let NSCalendarIdentifierIslamicUmmAlQura: String
struct NSCalendarUnit : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var era: NSCalendarUnit { get }
  static var year: NSCalendarUnit { get }
  static var month: NSCalendarUnit { get }
  static var day: NSCalendarUnit { get }
  static var hour: NSCalendarUnit { get }
  static var minute: NSCalendarUnit { get }
  static var second: NSCalendarUnit { get }
  static var weekday: NSCalendarUnit { get }
  static var weekdayOrdinal: NSCalendarUnit { get }
  @available(OSX 10.6, *)
  static var quarter: NSCalendarUnit { get }
  @available(OSX 10.7, *)
  static var weekOfMonth: NSCalendarUnit { get }
  @available(OSX 10.7, *)
  static var weekOfYear: NSCalendarUnit { get }
  @available(OSX 10.7, *)
  static var yearForWeekOfYear: NSCalendarUnit { get }
  @available(OSX 10.7, *)
  static var nanosecond: NSCalendarUnit { get }
  @available(OSX 10.7, *)
  static var calendar: NSCalendarUnit { get }
  @available(OSX 10.7, *)
  static var timeZone: NSCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarUnitEra instead")
  static var NSEraCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarUnitYear instead")
  static var NSYearCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarUnitMonth instead")
  static var NSMonthCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarUnitDay instead")
  static var NSDayCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarUnitHour instead")
  static var NSHourCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarUnitMinute instead")
  static var NSMinuteCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarUnitSecond instead")
  static var NSSecondCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarUnitWeekOfMonth or NSCalendarUnitWeekOfYear, depending on which you mean")
  static var NSWeekCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarUnitWeekday instead")
  static var NSWeekdayCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarUnitWeekdayOrdinal instead")
  static var NSWeekdayOrdinalCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.6, deprecated: 10.10, message: "Use NSCalendarUnitQuarter instead")
  static var NSQuarterCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.7, deprecated: 10.10, message: "Use NSCalendarUnitWeekOfMonth instead")
  static var NSWeekOfMonthCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.7, deprecated: 10.10, message: "Use NSCalendarUnitWeekOfYear instead")
  static var NSWeekOfYearCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.7, deprecated: 10.10, message: "Use NSCalendarUnitYearForWeekOfYear instead")
  static var NSYearForWeekOfYearCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.7, deprecated: 10.10, message: "Use NSCalendarUnitCalendar instead")
  static var NSCalendarCalendarUnit: NSCalendarUnit { get }
  @available(OSX, introduced: 10.7, deprecated: 10.10, message: "Use NSCalendarUnitTimeZone instead")
  static var NSTimeZoneCalendarUnit: NSCalendarUnit { get }
}
struct NSCalendarOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var wrapComponents: NSCalendarOptions { get }
  @available(OSX 10.9, *)
  static var matchStrictly: NSCalendarOptions { get }
  @available(OSX 10.9, *)
  static var searchBackwards: NSCalendarOptions { get }
  @available(OSX 10.9, *)
  static var matchPreviousTimePreservingSmallerUnits: NSCalendarOptions { get }
  @available(OSX 10.9, *)
  static var matchNextTimePreservingSmallerUnits: NSCalendarOptions { get }
  @available(OSX 10.9, *)
  static var matchNextTime: NSCalendarOptions { get }
  @available(OSX 10.9, *)
  static var matchFirst: NSCalendarOptions { get }
  @available(OSX 10.9, *)
  static var matchLast: NSCalendarOptions { get }
}
@available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSCalendarWrapComponents instead")
var NSWrapCalendarComponents: Int { get }
class NSCalendar : NSObject, NSCopying, NSSecureCoding {
  @discardableResult
  class func current() -> NSCalendar
  @available(OSX 10.5, *)
  @discardableResult
  class func autoupdatingCurrent() -> NSCalendar
  @available(OSX 10.9, *)
  /*not inherited*/ init?(identifier calendarIdentifierConstant: String)
  init?(calendarIdentifier ident: String)
  var calendarIdentifier: String { get }
  @NSCopying var locale: NSLocale?
  @NSCopying var timeZone: NSTimeZone
  var firstWeekday: Int
  var minimumDaysInFirstWeek: Int
  @available(OSX 10.7, *)
  var eraSymbols: [String] { get }
  @available(OSX 10.7, *)
  var longEraSymbols: [String] { get }
  @available(OSX 10.7, *)
  var monthSymbols: [String] { get }
  @available(OSX 10.7, *)
  var shortMonthSymbols: [String] { get }
  @available(OSX 10.7, *)
  var veryShortMonthSymbols: [String] { get }
  @available(OSX 10.7, *)
  var standaloneMonthSymbols: [String] { get }
  @available(OSX 10.7, *)
  var shortStandaloneMonthSymbols: [String] { get }
  @available(OSX 10.7, *)
  var veryShortStandaloneMonthSymbols: [String] { get }
  @available(OSX 10.7, *)
  var weekdaySymbols: [String] { get }
  @available(OSX 10.7, *)
  var shortWeekdaySymbols: [String] { get }
  @available(OSX 10.7, *)
  var veryShortWeekdaySymbols: [String] { get }
  @available(OSX 10.7, *)
  var standaloneWeekdaySymbols: [String] { get }
  @available(OSX 10.7, *)
  var shortStandaloneWeekdaySymbols: [String] { get }
  @available(OSX 10.7, *)
  var veryShortStandaloneWeekdaySymbols: [String] { get }
  @available(OSX 10.7, *)
  var quarterSymbols: [String] { get }
  @available(OSX 10.7, *)
  var shortQuarterSymbols: [String] { get }
  @available(OSX 10.7, *)
  var standaloneQuarterSymbols: [String] { get }
  @available(OSX 10.7, *)
  var shortStandaloneQuarterSymbols: [String] { get }
  @available(OSX 10.7, *)
  var amSymbol: String { get }
  @available(OSX 10.7, *)
  var pmSymbol: String { get }
  @discardableResult
  func minimumRange(of unit: NSCalendarUnit) -> NSRange
  @discardableResult
  func maximumRange(of unit: NSCalendarUnit) -> NSRange
  @discardableResult
  func range(of smaller: NSCalendarUnit, in larger: NSCalendarUnit, for date: NSDate) -> NSRange
  @discardableResult
  func ordinality(of smaller: NSCalendarUnit, in larger: NSCalendarUnit, for date: NSDate) -> Int
  @available(OSX 10.5, *)
  @discardableResult
  func range(of unit: NSCalendarUnit, start datep: AutoreleasingUnsafeMutablePointer<NSDate?>?, interval tip: UnsafeMutablePointer<NSTimeInterval>?, for date: NSDate) -> Bool
  @discardableResult
  func date(from comps: NSDateComponents) -> NSDate?
  @discardableResult
  func components(_ unitFlags: NSCalendarUnit, from date: NSDate) -> NSDateComponents
  @discardableResult
  func date(byAdding comps: NSDateComponents, to date: NSDate, options opts: NSCalendarOptions = []) -> NSDate?
  @discardableResult
  func components(_ unitFlags: NSCalendarUnit, from startingDate: NSDate, to resultDate: NSDate, options opts: NSCalendarOptions = []) -> NSDateComponents
  @available(OSX 10.9, *)
  func getEra(_ eraValuePointer: UnsafeMutablePointer<Int>?, year yearValuePointer: UnsafeMutablePointer<Int>?, month monthValuePointer: UnsafeMutablePointer<Int>?, day dayValuePointer: UnsafeMutablePointer<Int>?, from date: NSDate)
  @available(OSX 10.9, *)
  func getEra(_ eraValuePointer: UnsafeMutablePointer<Int>?, yearForWeekOfYear yearValuePointer: UnsafeMutablePointer<Int>?, weekOfYear weekValuePointer: UnsafeMutablePointer<Int>?, weekday weekdayValuePointer: UnsafeMutablePointer<Int>?, from date: NSDate)
  @available(OSX 10.9, *)
  func getHour(_ hourValuePointer: UnsafeMutablePointer<Int>?, minute minuteValuePointer: UnsafeMutablePointer<Int>?, second secondValuePointer: UnsafeMutablePointer<Int>?, nanosecond nanosecondValuePointer: UnsafeMutablePointer<Int>?, from date: NSDate)
  @available(OSX 10.9, *)
  @discardableResult
  func component(_ unit: NSCalendarUnit, from date: NSDate) -> Int
  @available(OSX 10.9, *)
  @discardableResult
  func date(era eraValue: Int, year yearValue: Int, month monthValue: Int, day dayValue: Int, hour hourValue: Int, minute minuteValue: Int, second secondValue: Int, nanosecond nanosecondValue: Int) -> NSDate?
  @available(OSX 10.9, *)
  @discardableResult
  func date(era eraValue: Int, yearForWeekOfYear yearValue: Int, weekOfYear weekValue: Int, weekday weekdayValue: Int, hour hourValue: Int, minute minuteValue: Int, second secondValue: Int, nanosecond nanosecondValue: Int) -> NSDate?
  @available(OSX 10.9, *)
  @discardableResult
  func startOfDay(for date: NSDate) -> NSDate
  @available(OSX 10.9, *)
  @discardableResult
  func components(in timezone: NSTimeZone, from date: NSDate) -> NSDateComponents
  @available(OSX 10.9, *)
  @discardableResult
  func compare(_ date1: NSDate, to date2: NSDate, toUnitGranularity unit: NSCalendarUnit) -> NSComparisonResult
  @available(OSX 10.9, *)
  @discardableResult
  func isDate(_ date1: NSDate, equalTo date2: NSDate, toUnitGranularity unit: NSCalendarUnit) -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  func isDate(_ date1: NSDate, inSameDayAs date2: NSDate) -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  func isDateInToday(_ date: NSDate) -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  func isDateInYesterday(_ date: NSDate) -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  func isDateInTomorrow(_ date: NSDate) -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  func isDateInWeekend(_ date: NSDate) -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  func range(ofWeekendStart datep: AutoreleasingUnsafeMutablePointer<NSDate?>?, interval tip: UnsafeMutablePointer<NSTimeInterval>?, containing date: NSDate) -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  func nextWeekendStart(_ datep: AutoreleasingUnsafeMutablePointer<NSDate?>?, interval tip: UnsafeMutablePointer<NSTimeInterval>?, options options: NSCalendarOptions = [], after date: NSDate) -> Bool
  @available(OSX 10.9, *)
  @discardableResult
  func components(_ unitFlags: NSCalendarUnit, from startingDateComp: NSDateComponents, to resultDateComp: NSDateComponents, options options: NSCalendarOptions = []) -> NSDateComponents
  @available(OSX 10.9, *)
  @discardableResult
  func date(byAdding unit: NSCalendarUnit, value value: Int, to date: NSDate, options options: NSCalendarOptions = []) -> NSDate?
  @available(OSX 10.9, *)
  func enumerateDates(startingAfter start: NSDate, matching comps: NSDateComponents, options opts: NSCalendarOptions = [], using block: (NSDate?, Bool, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(OSX 10.9, *)
  @discardableResult
  func nextDate(after date: NSDate, matching comps: NSDateComponents, options options: NSCalendarOptions = []) -> NSDate?
  @available(OSX 10.9, *)
  @discardableResult
  func nextDate(after date: NSDate, matching unit: NSCalendarUnit, value value: Int, options options: NSCalendarOptions = []) -> NSDate?
  @available(OSX 10.9, *)
  @discardableResult
  func nextDate(after date: NSDate, matchingHour hourValue: Int, minute minuteValue: Int, second secondValue: Int, options options: NSCalendarOptions = []) -> NSDate?
  @available(OSX 10.9, *)
  @discardableResult
  func date(bySettingUnit unit: NSCalendarUnit, value v: Int, of date: NSDate, options opts: NSCalendarOptions = []) -> NSDate?
  @available(OSX 10.9, *)
  @discardableResult
  func date(bySettingHour h: Int, minute m: Int, second s: Int, of date: NSDate, options opts: NSCalendarOptions = []) -> NSDate?
  @available(OSX 10.9, *)
  @discardableResult
  func date(_ date: NSDate, matchesComponents components: NSDateComponents) -> Bool
}
@available(OSX 10.9, *)
let NSCalendarDayChangedNotification: String
var NSDateComponentUndefined: Int { get }
@available(OSX, introduced: 10.4, deprecated: 10.10, message: "Use NSDateComponentUndefined instead")
var NSUndefinedDateComponent: Int { get }
class NSDateComponents : NSObject, NSCopying, NSSecureCoding {
  @available(OSX 10.7, *)
  @NSCopying var calendar: NSCalendar?
  @available(OSX 10.7, *)
  @NSCopying var timeZone: NSTimeZone?
  var era: Int
  var year: Int
  var month: Int
  var day: Int
  var hour: Int
  var minute: Int
  var second: Int
  @available(OSX 10.7, *)
  var nanosecond: Int
  var weekday: Int
  var weekdayOrdinal: Int
  @available(OSX 10.6, *)
  var quarter: Int
  @available(OSX 10.7, *)
  var weekOfMonth: Int
  @available(OSX 10.7, *)
  var weekOfYear: Int
  @available(OSX 10.7, *)
  var yearForWeekOfYear: Int
  @available(OSX 10.8, *)
  var isLeapMonth: Bool
  @available(OSX 10.7, *)
  @NSCopying var date: NSDate? { get }
  @available(OSX 10.9, *)
  func setValue(_ value: Int, forComponent unit: NSCalendarUnit)
  @available(OSX 10.9, *)
  @discardableResult
  func value(forComponent unit: NSCalendarUnit) -> Int
  @available(OSX 10.9, *)
  var isValidDate: Bool { get }
  @available(OSX 10.9, *)
  @discardableResult
  func isValidDate(in calendar: NSCalendar) -> Bool
}
