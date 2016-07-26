
struct NSByteCountFormatterUnits : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var useBytes: NSByteCountFormatterUnits { get }
  static var useKB: NSByteCountFormatterUnits { get }
  static var useMB: NSByteCountFormatterUnits { get }
  static var useGB: NSByteCountFormatterUnits { get }
  static var useTB: NSByteCountFormatterUnits { get }
  static var usePB: NSByteCountFormatterUnits { get }
  static var useEB: NSByteCountFormatterUnits { get }
  static var useZB: NSByteCountFormatterUnits { get }
  static var useYBOrHigher: NSByteCountFormatterUnits { get }
  static var useAll: NSByteCountFormatterUnits { get }
}
enum NSByteCountFormatterCountStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case file
  case memory
  case decimal
  case binary
}
@available(tvOS 6.0, *)
class NSByteCountFormatter : NSFormatter {
  @discardableResult
  class func string(fromByteCount byteCount: Int64, countStyle countStyle: NSByteCountFormatterCountStyle) -> String
  @discardableResult
  func stringFromByteCount(_ byteCount: Int64) -> String
  var allowedUnits: NSByteCountFormatterUnits
  var countStyle: NSByteCountFormatterCountStyle
  var allowsNonnumericFormatting: Bool
  var includesUnit: Bool
  var includesCount: Bool
  var includesActualByteCount: Bool
  var isAdaptive: Bool
  var zeroPadsFractionDigits: Bool
  @available(tvOS 8.0, *)
  var formattingContext: NSFormattingContext
}
