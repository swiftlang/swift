
@available(OSX 10.10, *)
enum NSDateComponentsFormatterUnitsStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case positional
  case abbreviated
  case short
  case full
  case spellOut
}
@available(OSX 10.10, *)
struct NSDateComponentsFormatterZeroFormattingBehavior : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var `default`: NSDateComponentsFormatterZeroFormattingBehavior { get }
  static var dropLeading: NSDateComponentsFormatterZeroFormattingBehavior { get }
  static var dropMiddle: NSDateComponentsFormatterZeroFormattingBehavior { get }
  static var dropTrailing: NSDateComponentsFormatterZeroFormattingBehavior { get }
  static var dropAll: NSDateComponentsFormatterZeroFormattingBehavior { get }
  static var pad: NSDateComponentsFormatterZeroFormattingBehavior { get }
}
@available(OSX 10.10, *)
class NSDateComponentsFormatter : NSFormatter {
  @discardableResult
  func string(from components: NSDateComponents) -> String?
  @discardableResult
  func string(from startDate: NSDate, to endDate: NSDate) -> String?
  @discardableResult
  func string(from ti: NSTimeInterval) -> String?
  @discardableResult
  class func localizedString(from components: NSDateComponents, unitsStyle unitsStyle: NSDateComponentsFormatterUnitsStyle) -> String?
  var unitsStyle: NSDateComponentsFormatterUnitsStyle
  var allowedUnits: NSCalendarUnit
  var zeroFormattingBehavior: NSDateComponentsFormatterZeroFormattingBehavior
  @NSCopying var calendar: NSCalendar?
  var allowsFractionalUnits: Bool
  var maximumUnitCount: Int
  var collapsesLargestUnit: Bool
  var includesApproximationPhrase: Bool
  var includesTimeRemainingPhrase: Bool
  var formattingContext: NSFormattingContext
}
