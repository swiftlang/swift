
class CLKTextProvider : NSObject, NSCopying {
  var tintColor: UIColor
}
class CLKSimpleTextProvider : CLKTextProvider {
  convenience init(text text: String)
  convenience init(text text: String, shortText shortText: String?)
  convenience init(text text: String, shortText shortText: String?, accessibilityLabel accessibilityLabel: String?)
  var text: String
  var shortText: String?
  var accessibilityLabel: String?
}
class CLKDateTextProvider : CLKTextProvider {
  convenience init(date date: NSDate, units calendarUnits: NSCalendarUnit)
  convenience init(date date: NSDate, units calendarUnits: NSCalendarUnit, timeZone timeZone: NSTimeZone?)
  var date: NSDate
  var calendarUnits: NSCalendarUnit
  var timeZone: NSTimeZone?
}
class CLKTimeTextProvider : CLKTextProvider {
  convenience init(date date: NSDate)
  convenience init(date date: NSDate, timeZone timeZone: NSTimeZone?)
  var date: NSDate
  var timeZone: NSTimeZone?
}
class CLKTimeIntervalTextProvider : CLKTextProvider {
  convenience init(start startDate: NSDate, end endDate: NSDate)
  convenience init(start startDate: NSDate, end endDate: NSDate, timeZone timeZone: NSTimeZone?)
  var startDate: NSDate
  var endDate: NSDate
  var timeZone: NSTimeZone?
}
enum CLKRelativeDateStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case natural
  case offset
  case timer
}
class CLKRelativeDateTextProvider : CLKTextProvider {
  convenience init(date date: NSDate, style style: CLKRelativeDateStyle, units calendarUnits: NSCalendarUnit)
  var date: NSDate
  var relativeDateStyle: CLKRelativeDateStyle
  var calendarUnits: NSCalendarUnit
}
