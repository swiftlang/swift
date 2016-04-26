
class NSTimeZone : NSObject, NSCopying, NSSecureCoding {
  var name: String { get }
  @NSCopying var data: NSData { get }
  @discardableResult
  func secondsFromGMT(for aDate: NSDate) -> Int
  @discardableResult
  func abbreviation(for aDate: NSDate) -> String?
  @discardableResult
  func isDaylightSavingTime(for aDate: NSDate) -> Bool
  @available(iOS 2.0, *)
  @discardableResult
  func daylightSavingTimeOffset(for aDate: NSDate) -> NSTimeInterval
  @available(iOS 2.0, *)
  @discardableResult
  func nextDaylightSavingTimeTransition(after aDate: NSDate) -> NSDate?
}
extension NSTimeZone {
  @discardableResult
  class func system() -> NSTimeZone
  class func resetSystemTimeZone()
  @discardableResult
  class func defaultTimeZone() -> NSTimeZone
  class func setDefaultTimeZone(_ aTimeZone: NSTimeZone)
  @discardableResult
  class func local() -> NSTimeZone
  @discardableResult
  class func knownTimeZoneNames() -> [String]
  @discardableResult
  class func abbreviationDictionary() -> [String : String]
  @available(iOS 4.0, *)
  class func setAbbreviationDictionary(_ dict: [String : String])
  @available(iOS 4.0, *)
  @discardableResult
  class func timeZoneDataVersion() -> String
  var secondsFromGMT: Int { get }
  var abbreviation: String? { get }
  var isDaylightSavingTime: Bool { get }
  @available(iOS 2.0, *)
  var daylightSavingTimeOffset: NSTimeInterval { get }
  @available(iOS 2.0, *)
  @NSCopying var nextDaylightSavingTimeTransition: NSDate? { get }
  @discardableResult
  func isEqual(to aTimeZone: NSTimeZone) -> Bool
  @available(iOS 2.0, *)
  @discardableResult
  func localizedName(_ style: NSTimeZoneNameStyle, locale locale: NSLocale?) -> String?
}
enum NSTimeZoneNameStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case standard
  case shortStandard
  case daylightSaving
  case shortDaylightSaving
  case generic
  case shortGeneric
}
extension NSTimeZone {
  init?(name tzName: String)
  init?(name tzName: String, data aData: NSData?)
  convenience init(forSecondsFromGMT seconds: Int)
  convenience init?(abbreviation abbreviation: String)
}
@available(iOS 2.0, *)
let NSSystemTimeZoneDidChangeNotification: String
