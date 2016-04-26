
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
  @available(iOS 2.0, *)
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
  @available(iOS 2.0, *)
  @discardableResult
  class func commonISOCurrencyCodes() -> [String]
  @available(iOS 2.0, *)
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
  @available(iOS 4.0, *)
  @discardableResult
  class func localeIdentifier(fromWindowsLocaleCode lcid: UInt32) -> String?
  @available(iOS 4.0, *)
  @discardableResult
  class func windowsLocaleCode(fromLocaleIdentifier localeIdentifier: String) -> UInt32
  @available(iOS 4.0, *)
  @discardableResult
  class func characterDirection(forLanguage isoLangCode: String) -> NSLocaleLanguageDirection
  @available(iOS 4.0, *)
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
@available(iOS 2.0, *)
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
@available(iOS 4.0, *)
let NSLocaleCollatorIdentifier: String
@available(iOS 4.0, *)
let NSLocaleQuotationBeginDelimiterKey: String
@available(iOS 4.0, *)
let NSLocaleQuotationEndDelimiterKey: String
@available(iOS 4.0, *)
let NSLocaleAlternateQuotationBeginDelimiterKey: String
@available(iOS 4.0, *)
let NSLocaleAlternateQuotationEndDelimiterKey: String
@available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use NSCalendarIdentifierGregorian instead")
let NSGregorianCalendar: String
@available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use NSCalendarIdentifierBuddhist instead")
let NSBuddhistCalendar: String
@available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use NSCalendarIdentifierChinese instead")
let NSChineseCalendar: String
@available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use NSCalendarIdentifierHebrew instead")
let NSHebrewCalendar: String
@available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use NSCalendarIdentifierIslamic instead")
let NSIslamicCalendar: String
@available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use NSCalendarIdentifierIslamicCivil instead")
let NSIslamicCivilCalendar: String
@available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use NSCalendarIdentifierJapanese instead")
let NSJapaneseCalendar: String
@available(iOS, introduced: 4.0, deprecated: 8.0, message: "Use NSCalendarIdentifierRepublicOfChina instead")
let NSRepublicOfChinaCalendar: String
@available(iOS, introduced: 4.0, deprecated: 8.0, message: "Use NSCalendarIdentifierPersian instead")
let NSPersianCalendar: String
@available(iOS, introduced: 4.0, deprecated: 8.0, message: "Use NSCalendarIdentifierIndian instead")
let NSIndianCalendar: String
@available(iOS, introduced: 4.0, deprecated: 8.0, message: "Use NSCalendarIdentifierISO8601 instead")
let NSISO8601Calendar: String
