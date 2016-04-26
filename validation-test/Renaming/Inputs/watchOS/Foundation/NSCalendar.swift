
@available(watchOS 2.0, *)
let NSCalendarIdentifierGregorian: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierBuddhist: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierChinese: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierCoptic: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierEthiopicAmeteMihret: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierEthiopicAmeteAlem: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierHebrew: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierISO8601: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierIndian: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierIslamic: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierIslamicCivil: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierJapanese: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierPersian: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierRepublicOfChina: String
@available(watchOS 2.0, *)
let NSCalendarIdentifierIslamicTabular: String
@available(watchOS 2.0, *)
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
  @available(watchOS 2.0, *)
  static var quarter: NSCalendarUnit { get }
  @available(watchOS 2.0, *)
  static var weekOfMonth: NSCalendarUnit { get }
  @available(watchOS 2.0, *)
  static var weekOfYear: NSCalendarUnit { get }
  @available(watchOS 2.0, *)
  static var yearForWeekOfYear: NSCalendarUnit { get }
  @available(watchOS 2.0, *)
  static var nanosecond: NSCalendarUnit { get }
  @available(watchOS 2.0, *)
  static var calendar: NSCalendarUnit { get }
  @available(watchOS 2.0, *)
  static var timeZone: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitEra instead")
  static var NSEraCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitYear instead")
  static var NSYearCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitMonth instead")
  static var NSMonthCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitDay instead")
  static var NSDayCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitHour instead")
  static var NSHourCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitMinute instead")
  static var NSMinuteCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitSecond instead")
  static var NSSecondCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitWeekOfMonth or NSCalendarUnitWeekOfYear, depending on which you mean")
  static var NSWeekCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitWeekday instead")
  static var NSWeekdayCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitWeekdayOrdinal instead")
  static var NSWeekdayOrdinalCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitQuarter instead")
  static var NSQuarterCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitWeekOfMonth instead")
  static var NSWeekOfMonthCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitWeekOfYear instead")
  static var NSWeekOfYearCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitYearForWeekOfYear instead")
  static var NSYearForWeekOfYearCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitCalendar instead")
  static var NSCalendarCalendarUnit: NSCalendarUnit { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarUnitTimeZone instead")
  static var NSTimeZoneCalendarUnit: NSCalendarUnit { get }
}
struct NSCalendarOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var wrapComponents: NSCalendarOptions { get }
  @available(watchOS 2.0, *)
  static var matchStrictly: NSCalendarOptions { get }
  @available(watchOS 2.0, *)
  static var searchBackwards: NSCalendarOptions { get }
  @available(watchOS 2.0, *)
  static var matchPreviousTimePreservingSmallerUnits: NSCalendarOptions { get }
  @available(watchOS 2.0, *)
  static var matchNextTimePreservingSmallerUnits: NSCalendarOptions { get }
  @available(watchOS 2.0, *)
  static var matchNextTime: NSCalendarOptions { get }
  @available(watchOS 2.0, *)
  static var matchFirst: NSCalendarOptions { get }
  @available(watchOS 2.0, *)
  static var matchLast: NSCalendarOptions { get }
}
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarWrapComponents instead")
var NSWrapCalendarComponents: Int { get }
class NSCalendar : NSObject, NSCopying, NSSecureCoding {
  @discardableResult
  class func current() -> NSCalendar
  @available(watchOS 2.0, *)
  @discardableResult
  class func autoupdatingCurrent() -> NSCalendar
  @available(watchOS 2.0, *)
  /*not inherited*/ init?(identifier calendarIdentifierConstant: String)
  init?(calendarIdentifier ident: String)
  var calendarIdentifier: String { get }
  @NSCopying var locale: NSLocale?
  @NSCopying var timeZone: NSTimeZone
  var firstWeekday: Int
  var minimumDaysInFirstWeek: Int
  @available(watchOS 2.0, *)
  var eraSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var longEraSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var monthSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var shortMonthSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var veryShortMonthSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var standaloneMonthSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var shortStandaloneMonthSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var veryShortStandaloneMonthSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var weekdaySymbols: [String] { get }
  @available(watchOS 2.0, *)
  var shortWeekdaySymbols: [String] { get }
  @available(watchOS 2.0, *)
  var veryShortWeekdaySymbols: [String] { get }
  @available(watchOS 2.0, *)
  var standaloneWeekdaySymbols: [String] { get }
  @available(watchOS 2.0, *)
  var shortStandaloneWeekdaySymbols: [String] { get }
  @available(watchOS 2.0, *)
  var veryShortStandaloneWeekdaySymbols: [String] { get }
  @available(watchOS 2.0, *)
  var quarterSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var shortQuarterSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var standaloneQuarterSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var shortStandaloneQuarterSymbols: [String] { get }
  @available(watchOS 2.0, *)
  var amSymbol: String { get }
  @available(watchOS 2.0, *)
  var pmSymbol: String { get }
  @discardableResult
  func minimumRange(of unit: NSCalendarUnit) -> NSRange
  @discardableResult
  func maximumRange(of unit: NSCalendarUnit) -> NSRange
  @discardableResult
  func range(of smaller: NSCalendarUnit, in larger: NSCalendarUnit, for date: NSDate) -> NSRange
  @discardableResult
  func ordinality(of smaller: NSCalendarUnit, in larger: NSCalendarUnit, for date: NSDate) -> Int
  @available(watchOS 2.0, *)
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
  @available(watchOS 2.0, *)
  func getEra(_ eraValuePointer: UnsafeMutablePointer<Int>?, year yearValuePointer: UnsafeMutablePointer<Int>?, month monthValuePointer: UnsafeMutablePointer<Int>?, day dayValuePointer: UnsafeMutablePointer<Int>?, from date: NSDate)
  @available(watchOS 2.0, *)
  func getEra(_ eraValuePointer: UnsafeMutablePointer<Int>?, yearForWeekOfYear yearValuePointer: UnsafeMutablePointer<Int>?, weekOfYear weekValuePointer: UnsafeMutablePointer<Int>?, weekday weekdayValuePointer: UnsafeMutablePointer<Int>?, from date: NSDate)
  @available(watchOS 2.0, *)
  func getHour(_ hourValuePointer: UnsafeMutablePointer<Int>?, minute minuteValuePointer: UnsafeMutablePointer<Int>?, second secondValuePointer: UnsafeMutablePointer<Int>?, nanosecond nanosecondValuePointer: UnsafeMutablePointer<Int>?, from date: NSDate)
  @available(watchOS 2.0, *)
  @discardableResult
  func component(_ unit: NSCalendarUnit, from date: NSDate) -> Int
  @available(watchOS 2.0, *)
  @discardableResult
  func date(era eraValue: Int, year yearValue: Int, month monthValue: Int, day dayValue: Int, hour hourValue: Int, minute minuteValue: Int, second secondValue: Int, nanosecond nanosecondValue: Int) -> NSDate?
  @available(watchOS 2.0, *)
  @discardableResult
  func date(era eraValue: Int, yearForWeekOfYear yearValue: Int, weekOfYear weekValue: Int, weekday weekdayValue: Int, hour hourValue: Int, minute minuteValue: Int, second secondValue: Int, nanosecond nanosecondValue: Int) -> NSDate?
  @available(watchOS 2.0, *)
  @discardableResult
  func startOfDay(for date: NSDate) -> NSDate
  @available(watchOS 2.0, *)
  @discardableResult
  func components(in timezone: NSTimeZone, from date: NSDate) -> NSDateComponents
  @available(watchOS 2.0, *)
  @discardableResult
  func compare(_ date1: NSDate, to date2: NSDate, toUnitGranularity unit: NSCalendarUnit) -> NSComparisonResult
  @available(watchOS 2.0, *)
  @discardableResult
  func isDate(_ date1: NSDate, equalTo date2: NSDate, toUnitGranularity unit: NSCalendarUnit) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func isDate(_ date1: NSDate, inSameDayAs date2: NSDate) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func isDateInToday(_ date: NSDate) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func isDateInYesterday(_ date: NSDate) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func isDateInTomorrow(_ date: NSDate) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func isDateInWeekend(_ date: NSDate) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func range(ofWeekendStart datep: AutoreleasingUnsafeMutablePointer<NSDate?>?, interval tip: UnsafeMutablePointer<NSTimeInterval>?, containing date: NSDate) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func nextWeekendStart(_ datep: AutoreleasingUnsafeMutablePointer<NSDate?>?, interval tip: UnsafeMutablePointer<NSTimeInterval>?, options options: NSCalendarOptions = [], after date: NSDate) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func components(_ unitFlags: NSCalendarUnit, from startingDateComp: NSDateComponents, to resultDateComp: NSDateComponents, options options: NSCalendarOptions = []) -> NSDateComponents
  @available(watchOS 2.0, *)
  @discardableResult
  func date(byAdding unit: NSCalendarUnit, value value: Int, to date: NSDate, options options: NSCalendarOptions = []) -> NSDate?
  @available(watchOS 2.0, *)
  func enumerateDates(startingAfter start: NSDate, matching comps: NSDateComponents, options opts: NSCalendarOptions = [], using block: (NSDate?, Bool, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(watchOS 2.0, *)
  @discardableResult
  func nextDate(after date: NSDate, matching comps: NSDateComponents, options options: NSCalendarOptions = []) -> NSDate?
  @available(watchOS 2.0, *)
  @discardableResult
  func nextDate(after date: NSDate, matching unit: NSCalendarUnit, value value: Int, options options: NSCalendarOptions = []) -> NSDate?
  @available(watchOS 2.0, *)
  @discardableResult
  func nextDate(after date: NSDate, matchingHour hourValue: Int, minute minuteValue: Int, second secondValue: Int, options options: NSCalendarOptions = []) -> NSDate?
  @available(watchOS 2.0, *)
  @discardableResult
  func date(bySettingUnit unit: NSCalendarUnit, value v: Int, of date: NSDate, options opts: NSCalendarOptions = []) -> NSDate?
  @available(watchOS 2.0, *)
  @discardableResult
  func date(bySettingHour h: Int, minute m: Int, second s: Int, of date: NSDate, options opts: NSCalendarOptions = []) -> NSDate?
  @available(watchOS 2.0, *)
  @discardableResult
  func date(_ date: NSDate, matchesComponents components: NSDateComponents) -> Bool
}
@available(watchOS 2.0, *)
let NSCalendarDayChangedNotification: String
var NSDateComponentUndefined: Int { get }
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSDateComponentUndefined instead")
var NSUndefinedDateComponent: Int { get }
class NSDateComponents : NSObject, NSCopying, NSSecureCoding {
  @available(watchOS 2.0, *)
  @NSCopying var calendar: NSCalendar?
  @available(watchOS 2.0, *)
  @NSCopying var timeZone: NSTimeZone?
  var era: Int
  var year: Int
  var month: Int
  var day: Int
  var hour: Int
  var minute: Int
  var second: Int
  @available(watchOS 2.0, *)
  var nanosecond: Int
  var weekday: Int
  var weekdayOrdinal: Int
  @available(watchOS 2.0, *)
  var quarter: Int
  @available(watchOS 2.0, *)
  var weekOfMonth: Int
  @available(watchOS 2.0, *)
  var weekOfYear: Int
  @available(watchOS 2.0, *)
  var yearForWeekOfYear: Int
  @available(watchOS 2.0, *)
  var isLeapMonth: Bool
  @available(watchOS 2.0, *)
  @NSCopying var date: NSDate? { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use weekOfMonth or weekOfYear, depending on which you mean")
  @discardableResult
  func week() -> Int
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use setWeekOfMonth: or setWeekOfYear:, depending on which you mean")
  func setWeek(_ v: Int)
  @available(watchOS 2.0, *)
  func setValue(_ value: Int, forComponent unit: NSCalendarUnit)
  @available(watchOS 2.0, *)
  @discardableResult
  func value(forComponent unit: NSCalendarUnit) -> Int
  @available(watchOS 2.0, *)
  var isValidDate: Bool { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func isValidDate(in calendar: NSCalendar) -> Bool
}
