
class NSLocale : NSObject, NSCopying, NSSecureCoding {
  @discardableResult
  func object(forKey key: AnyObject) -> AnyObject?
  @discardableResult
  func displayName(forKey key: AnyObject, value value: AnyObject) -> String?
  init(localeIdentifier string: String)
}
extension NSLocale {
  var localeIdentifier: String { get }
}
extension NSLocale {
  @available(watchOS 2.0, *)
  @discardableResult
  class func autoupdatingCurrent() -> NSLocale
  @discardableResult
  class func current() -> NSLocale
  @discardableResult
  class func system() -> NSLocale
}
extension NSLocale {
  @discardableResult
  class func availableLocaleIdentifiers() -> [String]
  @discardableResult
  class func isoLanguageCodes() -> [String]
  @discardableResult
  class func isoCountryCodes() -> [String]
  @discardableResult
  class func isoCurrencyCodes() -> [String]
  @available(watchOS 2.0, *)
  @discardableResult
  class func commonISOCurrencyCodes() -> [String]
  @available(watchOS 2.0, *)
  @discardableResult
  class func preferredLanguages() -> [String]
  @discardableResult
  class func components(fromLocaleIdentifier string: String) -> [String : String]
  @discardableResult
  class func localeIdentifier(fromComponents dict: [String : String]) -> String
  @discardableResult
  class func canonicalLocaleIdentifier(from string: String) -> String
  @discardableResult
  class func canonicalLanguageIdentifier(from string: String) -> String
  @available(watchOS 2.0, *)
  @discardableResult
  class func localeIdentifier(fromWindowsLocaleCode lcid: UInt32) -> String?
  @available(watchOS 2.0, *)
  @discardableResult
  class func windowsLocaleCode(fromLocaleIdentifier localeIdentifier: String) -> UInt32
  @available(watchOS 2.0, *)
  @discardableResult
  class func characterDirection(forLanguage isoLangCode: String) -> NSLocaleLanguageDirection
  @available(watchOS 2.0, *)
  @discardableResult
  class func lineDirection(forLanguage isoLangCode: String) -> NSLocaleLanguageDirection
}
enum NSLocaleLanguageDirection : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case unknown
  case leftToRight
  case rightToLeft
  case topToBottom
  case bottomToTop
}
@available(watchOS 2.0, *)
let NSCurrentLocaleDidChangeNotification: String
let NSLocaleIdentifier: String
let NSLocaleLanguageCode: String
let NSLocaleCountryCode: String
let NSLocaleScriptCode: String
let NSLocaleVariantCode: String
let NSLocaleExemplarCharacterSet: String
let NSLocaleCalendar: String
let NSLocaleCollationIdentifier: String
let NSLocaleUsesMetricSystem: String
let NSLocaleMeasurementSystem: String
let NSLocaleDecimalSeparator: String
let NSLocaleGroupingSeparator: String
let NSLocaleCurrencySymbol: String
let NSLocaleCurrencyCode: String
@available(watchOS 2.0, *)
let NSLocaleCollatorIdentifier: String
@available(watchOS 2.0, *)
let NSLocaleQuotationBeginDelimiterKey: String
@available(watchOS 2.0, *)
let NSLocaleQuotationEndDelimiterKey: String
@available(watchOS 2.0, *)
let NSLocaleAlternateQuotationBeginDelimiterKey: String
@available(watchOS 2.0, *)
let NSLocaleAlternateQuotationEndDelimiterKey: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierGregorian instead")
let NSGregorianCalendar: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierBuddhist instead")
let NSBuddhistCalendar: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierChinese instead")
let NSChineseCalendar: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierHebrew instead")
let NSHebrewCalendar: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierIslamic instead")
let NSIslamicCalendar: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierIslamicCivil instead")
let NSIslamicCivilCalendar: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierJapanese instead")
let NSJapaneseCalendar: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierRepublicOfChina instead")
let NSRepublicOfChinaCalendar: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierPersian instead")
let NSPersianCalendar: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierIndian instead")
let NSIndianCalendar: String
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSCalendarIdentifierISO8601 instead")
let NSISO8601Calendar: String
