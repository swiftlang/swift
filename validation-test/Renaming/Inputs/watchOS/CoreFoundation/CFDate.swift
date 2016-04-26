
typealias CFTimeInterval = Double
typealias CFAbsoluteTime = CFTimeInterval
@discardableResult
func CFAbsoluteTimeGetCurrent() -> CFAbsoluteTime
let kCFAbsoluteTimeIntervalSince1970: CFTimeInterval
let kCFAbsoluteTimeIntervalSince1904: CFTimeInterval
class CFDate {
}
@discardableResult
func CFDateGetTypeID() -> CFTypeID
@discardableResult
func CFDateCreate(_ allocator: CFAllocator!, _ at: CFAbsoluteTime) -> CFDate!
@discardableResult
func CFDateGetAbsoluteTime(_ theDate: CFDate!) -> CFAbsoluteTime
@discardableResult
func CFDateGetTimeIntervalSinceDate(_ theDate: CFDate!, _ otherDate: CFDate!) -> CFTimeInterval
@discardableResult
func CFDateCompare(_ theDate: CFDate!, _ otherDate: CFDate!, _ context: UnsafeMutablePointer<Void>!) -> CFComparisonResult
class CFTimeZone {
}
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
struct CFGregorianDate {
  var year: Int32
  var month: Int8
  var day: Int8
  var hour: Int8
  var minute: Int8
  var second: Double
  init()
  init(year year: Int32, month month: Int8, day day: Int8, hour hour: Int8, minute minute: Int8, second second: Double)
}
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
struct CFGregorianUnits {
  var years: Int32
  var months: Int32
  var days: Int32
  var hours: Int32
  var minutes: Int32
  var seconds: Double
  init()
  init(years years: Int32, months months: Int32, days days: Int32, hours hours: Int32, minutes minutes: Int32, seconds seconds: Double)
}
struct CFGregorianUnitFlags : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
  static var unitsYears: CFGregorianUnitFlags { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
  static var unitsMonths: CFGregorianUnitFlags { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
  static var unitsDays: CFGregorianUnitFlags { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
  static var unitsHours: CFGregorianUnitFlags { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
  static var unitsMinutes: CFGregorianUnitFlags { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
  static var unitsSeconds: CFGregorianUnitFlags { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
  static var allUnits: CFGregorianUnitFlags { get }
}
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
@discardableResult
func CFGregorianDateIsValid(_ gdate: CFGregorianDate, _ unitFlags: CFOptionFlags) -> Bool
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
@discardableResult
func CFGregorianDateGetAbsoluteTime(_ gdate: CFGregorianDate, _ tz: CFTimeZone!) -> CFAbsoluteTime
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
@discardableResult
func CFAbsoluteTimeGetGregorianDate(_ at: CFAbsoluteTime, _ tz: CFTimeZone!) -> CFGregorianDate
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
@discardableResult
func CFAbsoluteTimeAddGregorianUnits(_ at: CFAbsoluteTime, _ tz: CFTimeZone!, _ units: CFGregorianUnits) -> CFAbsoluteTime
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
@discardableResult
func CFAbsoluteTimeGetDifferenceAsGregorianUnits(_ at1: CFAbsoluteTime, _ at2: CFAbsoluteTime, _ tz: CFTimeZone!, _ unitFlags: CFOptionFlags) -> CFGregorianUnits
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
@discardableResult
func CFAbsoluteTimeGetDayOfWeek(_ at: CFAbsoluteTime, _ tz: CFTimeZone!) -> Int32
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
@discardableResult
func CFAbsoluteTimeGetDayOfYear(_ at: CFAbsoluteTime, _ tz: CFTimeZone!) -> Int32
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use CFCalendar or NSCalendar API instead")
@discardableResult
func CFAbsoluteTimeGetWeekOfYear(_ at: CFAbsoluteTime, _ tz: CFTimeZone!) -> Int32
