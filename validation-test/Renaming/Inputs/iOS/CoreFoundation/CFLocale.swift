
class CFLocale {
}
@discardableResult
func CFLocaleGetTypeID() -> CFTypeID
@discardableResult
func CFLocaleGetSystem() -> CFLocale!
@discardableResult
func CFLocaleCopyCurrent() -> CFLocale!
@discardableResult
func CFLocaleCopyAvailableLocaleIdentifiers() -> CFArray!
@discardableResult
func CFLocaleCopyISOLanguageCodes() -> CFArray!
@discardableResult
func CFLocaleCopyISOCountryCodes() -> CFArray!
@discardableResult
func CFLocaleCopyISOCurrencyCodes() -> CFArray!
@available(iOS 2.0, *)
@discardableResult
func CFLocaleCopyCommonISOCurrencyCodes() -> CFArray!
@available(iOS 2.0, *)
@discardableResult
func CFLocaleCopyPreferredLanguages() -> CFArray!
@discardableResult
func CFLocaleCreateCanonicalLanguageIdentifierFromString(_ allocator: CFAllocator!, _ localeIdentifier: CFString!) -> CFString!
@discardableResult
func CFLocaleCreateCanonicalLocaleIdentifierFromString(_ allocator: CFAllocator!, _ localeIdentifier: CFString!) -> CFString!
@discardableResult
func CFLocaleCreateCanonicalLocaleIdentifierFromScriptManagerCodes(_ allocator: CFAllocator!, _ lcode: LangCode, _ rcode: RegionCode) -> CFString!
@available(iOS 4.0, *)
@discardableResult
func CFLocaleCreateLocaleIdentifierFromWindowsLocaleCode(_ allocator: CFAllocator!, _ lcid: UInt32) -> CFString!
@available(iOS 4.0, *)
@discardableResult
func CFLocaleGetWindowsLocaleCodeFromLocaleIdentifier(_ localeIdentifier: CFString!) -> UInt32
enum CFLocaleLanguageDirection : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case unknown
  case leftToRight
  case rightToLeft
  case topToBottom
  case bottomToTop
}
@available(iOS 4.0, *)
@discardableResult
func CFLocaleGetLanguageCharacterDirection(_ isoLangCode: CFString!) -> CFLocaleLanguageDirection
@available(iOS 4.0, *)
@discardableResult
func CFLocaleGetLanguageLineDirection(_ isoLangCode: CFString!) -> CFLocaleLanguageDirection
@discardableResult
func CFLocaleCreateComponentsFromLocaleIdentifier(_ allocator: CFAllocator!, _ localeID: CFString!) -> CFDictionary!
@discardableResult
func CFLocaleCreateLocaleIdentifierFromComponents(_ allocator: CFAllocator!, _ dictionary: CFDictionary!) -> CFString!
@discardableResult
func CFLocaleCreate(_ allocator: CFAllocator!, _ localeIdentifier: CFString!) -> CFLocale!
@discardableResult
func CFLocaleCreateCopy(_ allocator: CFAllocator!, _ locale: CFLocale!) -> CFLocale!
@discardableResult
func CFLocaleGetIdentifier(_ locale: CFLocale!) -> CFString!
@discardableResult
func CFLocaleGetValue(_ locale: CFLocale!, _ key: CFString!) -> CFTypeRef!
@discardableResult
func CFLocaleCopyDisplayNameForPropertyValue(_ displayLocale: CFLocale!, _ key: CFString!, _ value: CFString!) -> CFString!
@available(iOS 2.0, *)
let kCFLocaleCurrentLocaleDidChangeNotification: CFString!
let kCFLocaleIdentifier: CFString!
let kCFLocaleLanguageCode: CFString!
let kCFLocaleCountryCode: CFString!
let kCFLocaleScriptCode: CFString!
let kCFLocaleVariantCode: CFString!
let kCFLocaleExemplarCharacterSet: CFString!
let kCFLocaleCalendarIdentifier: CFString!
let kCFLocaleCalendar: CFString!
let kCFLocaleCollationIdentifier: CFString!
let kCFLocaleUsesMetricSystem: CFString!
let kCFLocaleMeasurementSystem: CFString!
let kCFLocaleDecimalSeparator: CFString!
let kCFLocaleGroupingSeparator: CFString!
let kCFLocaleCurrencySymbol: CFString!
let kCFLocaleCurrencyCode: CFString!
@available(iOS 4.0, *)
let kCFLocaleCollatorIdentifier: CFString!
@available(iOS 4.0, *)
let kCFLocaleQuotationBeginDelimiterKey: CFString!
@available(iOS 4.0, *)
let kCFLocaleQuotationEndDelimiterKey: CFString!
@available(iOS 4.0, *)
let kCFLocaleAlternateQuotationBeginDelimiterKey: CFString!
@available(iOS 4.0, *)
let kCFLocaleAlternateQuotationEndDelimiterKey: CFString!
let kCFGregorianCalendar: CFString!
let kCFBuddhistCalendar: CFString!
let kCFChineseCalendar: CFString!
let kCFHebrewCalendar: CFString!
let kCFIslamicCalendar: CFString!
let kCFIslamicCivilCalendar: CFString!
let kCFJapaneseCalendar: CFString!
@available(iOS 4.0, *)
let kCFRepublicOfChinaCalendar: CFString!
@available(iOS 4.0, *)
let kCFPersianCalendar: CFString!
@available(iOS 4.0, *)
let kCFIndianCalendar: CFString!
@available(iOS 4.0, *)
let kCFISO8601Calendar: CFString!
@available(iOS 8.0, *)
let kCFIslamicTabularCalendar: CFString!
@available(iOS 8.0, *)
let kCFIslamicUmmAlQuraCalendar: CFString!
