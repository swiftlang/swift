
class CFDateFormatter {
}
@available(tvOS 4.0, *)
@discardableResult
func CFDateFormatterCreateDateFormatFromTemplate(_ allocator: CFAllocator!, _ tmplate: CFString!, _ options: CFOptionFlags, _ locale: CFLocale!) -> CFString!
@discardableResult
func CFDateFormatterGetTypeID() -> CFTypeID
enum CFDateFormatterStyle : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case noStyle
  case shortStyle
  case mediumStyle
  case longStyle
  case fullStyle
}
@discardableResult
func CFDateFormatterCreate(_ allocator: CFAllocator!, _ locale: CFLocale!, _ dateStyle: CFDateFormatterStyle, _ timeStyle: CFDateFormatterStyle) -> CFDateFormatter!
@discardableResult
func CFDateFormatterGetLocale(_ formatter: CFDateFormatter!) -> CFLocale!
@discardableResult
func CFDateFormatterGetDateStyle(_ formatter: CFDateFormatter!) -> CFDateFormatterStyle
@discardableResult
func CFDateFormatterGetTimeStyle(_ formatter: CFDateFormatter!) -> CFDateFormatterStyle
@discardableResult
func CFDateFormatterGetFormat(_ formatter: CFDateFormatter!) -> CFString!
func CFDateFormatterSetFormat(_ formatter: CFDateFormatter!, _ formatString: CFString!)
@discardableResult
func CFDateFormatterCreateStringWithDate(_ allocator: CFAllocator!, _ formatter: CFDateFormatter!, _ date: CFDate!) -> CFString!
@discardableResult
func CFDateFormatterCreateStringWithAbsoluteTime(_ allocator: CFAllocator!, _ formatter: CFDateFormatter!, _ at: CFAbsoluteTime) -> CFString!
@discardableResult
func CFDateFormatterCreateDateFromString(_ allocator: CFAllocator!, _ formatter: CFDateFormatter!, _ string: CFString!, _ rangep: UnsafeMutablePointer<CFRange>!) -> CFDate!
@discardableResult
func CFDateFormatterGetAbsoluteTimeFromString(_ formatter: CFDateFormatter!, _ string: CFString!, _ rangep: UnsafeMutablePointer<CFRange>!, _ atp: UnsafeMutablePointer<CFAbsoluteTime>!) -> Bool
func CFDateFormatterSetProperty(_ formatter: CFDateFormatter!, _ key: CFString!, _ value: CFTypeRef!)
@discardableResult
func CFDateFormatterCopyProperty(_ formatter: CFDateFormatter!, _ key: CFString!) -> CFTypeRef!
let kCFDateFormatterIsLenient: CFString!
let kCFDateFormatterTimeZone: CFString!
let kCFDateFormatterCalendarName: CFString!
let kCFDateFormatterDefaultFormat: CFString!
let kCFDateFormatterTwoDigitStartDate: CFString!
let kCFDateFormatterDefaultDate: CFString!
let kCFDateFormatterCalendar: CFString!
let kCFDateFormatterEraSymbols: CFString!
let kCFDateFormatterMonthSymbols: CFString!
let kCFDateFormatterShortMonthSymbols: CFString!
let kCFDateFormatterWeekdaySymbols: CFString!
let kCFDateFormatterShortWeekdaySymbols: CFString!
let kCFDateFormatterAMSymbol: CFString!
let kCFDateFormatterPMSymbol: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterLongEraSymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterVeryShortMonthSymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterStandaloneMonthSymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterShortStandaloneMonthSymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterVeryShortStandaloneMonthSymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterVeryShortWeekdaySymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterStandaloneWeekdaySymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterShortStandaloneWeekdaySymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterVeryShortStandaloneWeekdaySymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterQuarterSymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterShortQuarterSymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterStandaloneQuarterSymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterShortStandaloneQuarterSymbols: CFString!
@available(tvOS 2.0, *)
let kCFDateFormatterGregorianStartDate: CFString!
@available(tvOS 4.0, *)
let kCFDateFormatterDoesRelativeDateFormattingKey: CFString!
