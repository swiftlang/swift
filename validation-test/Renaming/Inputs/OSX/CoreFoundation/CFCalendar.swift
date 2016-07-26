
class CFCalendar {
}
@discardableResult
func CFCalendarGetTypeID() -> CFTypeID
@discardableResult
func CFCalendarCopyCurrent() -> CFCalendar!
@discardableResult
func CFCalendarCreateWithIdentifier(_ allocator: CFAllocator!, _ identifier: CFString!) -> CFCalendar!
@discardableResult
func CFCalendarGetIdentifier(_ calendar: CFCalendar!) -> CFString!
@discardableResult
func CFCalendarCopyLocale(_ calendar: CFCalendar!) -> CFLocale!
func CFCalendarSetLocale(_ calendar: CFCalendar!, _ locale: CFLocale!)
@discardableResult
func CFCalendarCopyTimeZone(_ calendar: CFCalendar!) -> CFTimeZone!
func CFCalendarSetTimeZone(_ calendar: CFCalendar!, _ tz: CFTimeZone!)
@discardableResult
func CFCalendarGetFirstWeekday(_ calendar: CFCalendar!) -> CFIndex
func CFCalendarSetFirstWeekday(_ calendar: CFCalendar!, _ wkdy: CFIndex)
@discardableResult
func CFCalendarGetMinimumDaysInFirstWeek(_ calendar: CFCalendar!) -> CFIndex
func CFCalendarSetMinimumDaysInFirstWeek(_ calendar: CFCalendar!, _ mwd: CFIndex)
struct CFCalendarUnit : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var era: CFCalendarUnit { get }
  static var year: CFCalendarUnit { get }
  static var month: CFCalendarUnit { get }
  static var day: CFCalendarUnit { get }
  static var hour: CFCalendarUnit { get }
  static var minute: CFCalendarUnit { get }
  static var second: CFCalendarUnit { get }
  @available(OSX, introduced: 10.4, deprecated: 10.10)
  static var week: CFCalendarUnit { get }
  static var weekday: CFCalendarUnit { get }
  static var weekdayOrdinal: CFCalendarUnit { get }
  @available(OSX 10.6, *)
  static var quarter: CFCalendarUnit { get }
  @available(OSX 10.7, *)
  static var weekOfMonth: CFCalendarUnit { get }
  @available(OSX 10.7, *)
  static var weekOfYear: CFCalendarUnit { get }
  @available(OSX 10.7, *)
  static var yearForWeekOfYear: CFCalendarUnit { get }
}
@discardableResult
func CFCalendarGetMinimumRangeOfUnit(_ calendar: CFCalendar!, _ unit: CFCalendarUnit) -> CFRange
@discardableResult
func CFCalendarGetMaximumRangeOfUnit(_ calendar: CFCalendar!, _ unit: CFCalendarUnit) -> CFRange
@discardableResult
func CFCalendarGetRangeOfUnit(_ calendar: CFCalendar!, _ smallerUnit: CFCalendarUnit, _ biggerUnit: CFCalendarUnit, _ at: CFAbsoluteTime) -> CFRange
@discardableResult
func CFCalendarGetOrdinalityOfUnit(_ calendar: CFCalendar!, _ smallerUnit: CFCalendarUnit, _ biggerUnit: CFCalendarUnit, _ at: CFAbsoluteTime) -> CFIndex
@available(OSX 10.5, *)
@discardableResult
func CFCalendarGetTimeRangeOfUnit(_ calendar: CFCalendar!, _ unit: CFCalendarUnit, _ at: CFAbsoluteTime, _ startp: UnsafeMutablePointer<CFAbsoluteTime>!, _ tip: UnsafeMutablePointer<CFTimeInterval>!) -> Bool
var kCFCalendarComponentsWrap: CFOptionFlags { get }
