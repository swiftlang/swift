//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module
import _SwiftFoundationOverlayShims

/**
 `Locale` encapsulates information about linguistic, cultural, and technological conventions and standards. Examples of information encapsulated by a locale include the symbol used for the decimal separator in numbers and the way dates are formatted.
 
 Locales are typically used to provide, format, and interpret information about and according to the user's customs and preferences. They are frequently used in conjunction with formatters. Although you can use many locales, you usually use the one associated with the current user.
*/
public struct Locale : Hashable, Equatable, ReferenceConvertible {
    public typealias ReferenceType = NSLocale
    
    public typealias LanguageDirection = NSLocale.LanguageDirection
    
    fileprivate var _wrapped : NSLocale
    private var _autoupdating : Bool

    /// Returns a locale which tracks the user's current preferences.
    ///
    /// If mutated, this Locale will no longer track the user's preferences.
    ///
    /// - note: The autoupdating Locale will only compare equal to another autoupdating Locale.
    public static var autoupdatingCurrent : Locale {
        return Locale(adoptingReference: __NSLocaleAutoupdating() as! NSLocale, autoupdating: true)
    }
    
    /// Returns the user's current locale.
    public static var current : Locale {
        return Locale(adoptingReference: __NSLocaleCurrent() as! NSLocale, autoupdating: false)
    }
    
    @available(*, unavailable, message: "Consider using the user's locale or nil instead, depending on use case")
    public static var system : Locale { fatalError() }
    
    // MARK: -
    //
    
    /// Return a locale with the specified identifier.
    public init(identifier: String) {
        _wrapped = NSLocale(localeIdentifier: identifier)
        _autoupdating = false
    }
    
    fileprivate init(reference: NSLocale) {
        _wrapped = reference.copy() as! NSLocale
        if __NSLocaleIsAutoupdating(reference) {
            _autoupdating = true
        } else {
            _autoupdating = false
        }
    }
    
    private init(adoptingReference reference: NSLocale, autoupdating: Bool) {
        _wrapped = reference
        _autoupdating = autoupdating
    }

    // MARK: -
    //
    
    /// Returns a localized string for a specified identifier.
    ///
    /// For example, in the "en" locale, the result for `"es"` is `"Spanish"`.
    public func localizedString(forIdentifier identifier: String) -> String? {
        return _wrapped.displayName(forKey: .identifier, value: identifier)
    }
    
    /// Returns a localized string for a specified language code.
    ///
    /// For example, in the "en" locale, the result for `"es"` is `"Spanish"`.
    public func localizedString(forLanguageCode languageCode: String) -> String? {
        return _wrapped.displayName(forKey: .languageCode, value: languageCode)
    }

    /// Returns a localized string for a specified region code.
    ///
    /// For example, in the "en" locale, the result for `"fr"` is `"France"`.
    public func localizedString(forRegionCode regionCode: String) -> String? {
        return _wrapped.displayName(forKey: .countryCode, value: regionCode)
    }
    
    /// Returns a localized string for a specified script code.
    ///
    /// For example, in the "en" locale, the result for `"Hans"` is `"Simplified Han"`.
    public func localizedString(forScriptCode scriptCode: String) -> String? {
        return _wrapped.displayName(forKey: .scriptCode, value: scriptCode)
    }

    /// Returns a localized string for a specified variant code.
    ///
    /// For example, in the "en" locale, the result for `"POSIX"` is `"Computer"`.
    public func localizedString(forVariantCode variantCode: String) -> String? {
        return _wrapped.displayName(forKey: .variantCode, value: variantCode)
    }
    
    /// Returns a localized string for a specified `Calendar.Identifier`.
    ///
    /// For example, in the "en" locale, the result for `.buddhist` is `"Buddhist Calendar"`.
    public func localizedString(for calendarIdentifier: Calendar.Identifier) -> String? {
        // NSLocale doesn't export a constant for this
        let result = CFLocaleCopyDisplayNameForPropertyValue(unsafeBitCast(_wrapped, to: CFLocale.self), .calendarIdentifier, Calendar._toNSCalendarIdentifier(calendarIdentifier).rawValue as CFString) as String
        return result
    }

    /// Returns a localized string for a specified ISO 4217 currency code.
    ///
    /// For example, in the "en" locale, the result for `"USD"` is `"US Dollar"`.
    /// - seealso: `Locale.isoCurrencyCodes`
    public func localizedString(forCurrencyCode currencyCode: String) -> String? {
        return _wrapped.displayName(forKey: .currencyCode, value: currencyCode)
    }

    /// Returns a localized string for a specified ICU collation identifier.
    ///
    /// For example, in the "en" locale, the result for `"phonebook"` is `"Phonebook Sort Order"`.
    public func localizedString(forCollationIdentifier collationIdentifier: String) -> String? {
        return _wrapped.displayName(forKey: .collationIdentifier, value: collationIdentifier)
    }

    /// Returns a localized string for a specified ICU collator identifier.
    public func localizedString(forCollatorIdentifier collatorIdentifier: String) -> String? {
        return _wrapped.displayName(forKey: .collatorIdentifier, value: collatorIdentifier)
    }

    // MARK: -
    //

    /// Returns the identifier of the locale.
    public var identifier: String {
        return _wrapped.localeIdentifier
    }
    
    /// Returns the language code of the locale, or nil if has none.
    ///
    /// For example, for the locale "zh-Hant-HK", returns "zh".
    public var languageCode: String? {
        return _wrapped.object(forKey: .languageCode) as? String
    }

    /// Returns the region code of the locale, or nil if it has none.
    ///
    /// For example, for the locale "zh-Hant-HK", returns "HK".
    public var regionCode: String? {
        // n.b. this is called countryCode in ObjC
        if let result = _wrapped.object(forKey: .countryCode) as? String {
            if result.isEmpty {
                return nil
            } else {
                return result
            }
        } else {
            return nil
        }
    }
    
    /// Returns the script code of the locale, or nil if has none.
    ///
    /// For example, for the locale "zh-Hant-HK", returns "Hant".
    public var scriptCode: String? {
        return _wrapped.object(forKey: .scriptCode) as? String
    }
    
    /// Returns the variant code for the locale, or nil if it has none.
    ///
    /// For example, for the locale "en_POSIX", returns "POSIX".
    public var variantCode: String? {
        if let result = _wrapped.object(forKey: .variantCode) as? String {
            if result.isEmpty {
                return nil
            } else {
                return result
            }
        } else {
            return nil
        }
    }
    
    /// Returns the exemplar character set for the locale, or nil if has none.
    public var exemplarCharacterSet: CharacterSet? {
        return _wrapped.object(forKey: .exemplarCharacterSet) as? CharacterSet
    }
    
    /// Returns the calendar for the locale, or the Gregorian calendar as a fallback.
    public var calendar: Calendar {
        // NSLocale should not return nil here
        if let result = _wrapped.object(forKey: .calendar) as? Calendar {
            return result
        } else {
            return Calendar(identifier: .gregorian)
        }
    }
    
    /// Returns the collation identifier for the locale, or nil if it has none.
    ///
    /// For example, for the locale "en_US@collation=phonebook", returns "phonebook".
    public var collationIdentifier: String? {
        return _wrapped.object(forKey: .collationIdentifier) as? String
    }
    
    /// Returns true if the locale uses the metric system.
    ///
    /// -seealso: MeasurementFormatter
    public var usesMetricSystem: Bool {
        // NSLocale should not return nil here, but just in case
        if let result = (_wrapped.object(forKey: .usesMetricSystem) as? NSNumber)?.boolValue {
            return result
        } else {
            return false
        }
    }
    
    /// Returns the decimal separator of the locale.
    ///
    /// For example, for "en_US", returns ".".
    public var decimalSeparator: String? {
        return _wrapped.object(forKey: .decimalSeparator) as? String
    }
    
    /// Returns the grouping separator of the locale.
    ///
    /// For example, for "en_US", returns ",".
    public var groupingSeparator: String? {
        return _wrapped.object(forKey: .groupingSeparator) as? String
    }
    
    /// Returns the currency symbol of the locale.
    ///
    /// For example, for "zh-Hant-HK", returns "HK$".
    public var currencySymbol: String? {
        return _wrapped.object(forKey: .currencySymbol) as? String
    }
    
    /// Returns the currency code of the locale.
    ///
    /// For example, for "zh-Hant-HK", returns "HKD".
    public var currencyCode: String? {
        return _wrapped.object(forKey: .currencyCode) as? String
    }
    
    /// Returns the collator identifier of the locale.
    public var collatorIdentifier: String? {
        return _wrapped.object(forKey: .collatorIdentifier) as? String
    }
    
    /// Returns the quotation begin delimiter of the locale.
    ///
    /// For example, returns `“` for "en_US", and `「` for "zh-Hant-HK".
    public var quotationBeginDelimiter: String? {
        return _wrapped.object(forKey: .quotationBeginDelimiterKey) as? String
    }
    
    /// Returns the quotation end delimiter of the locale.
    ///
    /// For example, returns `”` for "en_US", and `」` for "zh-Hant-HK".
    public var quotationEndDelimiter: String? {
        return _wrapped.object(forKey: .quotationEndDelimiterKey) as? String
    }
    
    /// Returns the alternate quotation begin delimiter of the locale.
    ///
    /// For example, returns `‘` for "en_US", and `『` for "zh-Hant-HK".
    public var alternateQuotationBeginDelimiter: String? {
        return _wrapped.object(forKey: .alternateQuotationBeginDelimiterKey) as? String
    }
    
    /// Returns the alternate quotation end delimiter of the locale.
    ///
    /// For example, returns `’` for "en_US", and `』` for "zh-Hant-HK".
    public var alternateQuotationEndDelimiter: String? {
        return _wrapped.object(forKey: .alternateQuotationEndDelimiterKey) as? String
    }
    
    // MARK: -
    // 
    
    /// Returns a list of available `Locale` identifiers.
    public static var availableIdentifiers: [String] {
        return NSLocale.availableLocaleIdentifiers
    }
    
    /// Returns a list of available `Locale` language codes.
    public static var isoLanguageCodes: [String] {
        return NSLocale.isoLanguageCodes
    }
    
    /// Returns a list of available `Locale` region codes.
    public static var isoRegionCodes: [String] {
        // This was renamed from Obj-C
        return NSLocale.isoCountryCodes
    }
    
    /// Returns a list of available `Locale` currency codes.
    public static var isoCurrencyCodes: [String] {
        return NSLocale.isoCurrencyCodes
    }
    
    /// Returns a list of common `Locale` currency codes.
    public static var commonISOCurrencyCodes: [String] {
        return NSLocale.commonISOCurrencyCodes
    }
    
    /// Returns a list of the user's preferred languages.
    ///
    /// - note: `Bundle` is responsible for determining the language that your application will run in, based on the result of this API and combined with the languages your application supports.
    /// - seealso: `Bundle.preferredLocalizations(from:)`
    /// - seealso: `Bundle.preferredLocalizations(from:forPreferences:)`
    public static var preferredLanguages: [String] {
        return NSLocale.preferredLanguages
    }
    
    /// Returns a dictionary that splits an identifier into its component pieces.
    public static func components(fromIdentifier string: String) -> [String : String] {
        return NSLocale.components(fromLocaleIdentifier: string)
    }
    
    /// Constructs an identifier from a dictionary of components.
    public static func identifier(fromComponents components: [String : String]) -> String {
        return NSLocale.localeIdentifier(fromComponents: components)
    }
    
    /// Returns a canonical identifier from the given string.
    public static func canonicalIdentifier(from string: String) -> String {
        return NSLocale.canonicalLocaleIdentifier(from: string)
    }
    
    /// Returns a canonical language identifier from the given string.
    public static func canonicalLanguageIdentifier(from string: String) -> String {
        return NSLocale.canonicalLanguageIdentifier(from: string)
    }
    
    /// Returns the `Locale` identifier from a given Windows locale code, or nil if it could not be converted.
    public static func identifier(fromWindowsLocaleCode code: Int) -> String? {
        return NSLocale.localeIdentifier(fromWindowsLocaleCode: UInt32(code))
    }
    
    /// Returns the Windows locale code from a given identifier, or nil if it could not be converted.
    public static func windowsLocaleCode(fromIdentifier identifier: String) -> Int? {
        let result = NSLocale.windowsLocaleCode(fromLocaleIdentifier: identifier)
        if result == 0 {
            return nil
        } else {
            return Int(result)
        }
    }
    
    /// Returns the character direction for a specified language code.
    public static func characterDirection(forLanguage isoLangCode: String) -> Locale.LanguageDirection {
        return NSLocale.characterDirection(forLanguage: isoLangCode)
    }
    
    /// Returns the line direction for a specified language code.
    public static func lineDirection(forLanguage isoLangCode: String) -> Locale.LanguageDirection {
        return NSLocale.lineDirection(forLanguage: isoLangCode)
    }
    
    // MARK: -
    
    @available(*, unavailable, renamed: "init(identifier:)")
    public init(localeIdentifier: String) { fatalError() }
    
    @available(*, unavailable, renamed: "identifier")
    public var localeIdentifier: String { fatalError() }
    
    @available(*, unavailable, renamed: "localizedString(forIdentifier:)")
    public func localizedString(forLocaleIdentifier localeIdentifier: String) -> String { fatalError() }
    
    @available(*, unavailable, renamed: "availableIdentifiers")
    public static var availableLocaleIdentifiers: [String] { fatalError() }
    
    @available(*, unavailable, renamed: "components(fromIdentifier:)")
    public static func components(fromLocaleIdentifier string: String) -> [String : String] { fatalError() }
    
    @available(*, unavailable, renamed: "identifier(fromComponents:)")
    public static func localeIdentifier(fromComponents dict: [String : String]) -> String { fatalError() }
    
    @available(*, unavailable, renamed: "canonicalIdentifier(from:)")
    public static func canonicalLocaleIdentifier(from string: String) -> String { fatalError() }
    
    @available(*, unavailable, renamed: "identifier(fromWindowsLocaleCode:)")
    public static func localeIdentifier(fromWindowsLocaleCode lcid: UInt32) -> String? { fatalError() }
    
    @available(*, unavailable, renamed: "windowsLocaleCode(fromIdentifier:)")
    public static func windowsLocaleCode(fromLocaleIdentifier localeIdentifier: String) -> UInt32 { fatalError() }
    
    @available(*, unavailable, message: "use regionCode instead")
    public var countryCode: String { fatalError() }
    
    @available(*, unavailable, message: "use localizedString(forRegionCode:) instead")
    public func localizedString(forCountryCode countryCode: String) -> String { fatalError() }

    @available(*, unavailable, renamed: "isoRegionCodes")
    public static var isoCountryCodes: [String] { fatalError() }

    // MARK: -
    //
    
    public var hashValue : Int {
        if _autoupdating {
            return 1
        } else {
            return _wrapped.hash
        }
    }

    public static func ==(lhs: Locale, rhs: Locale) -> Bool {
        if lhs._autoupdating || rhs._autoupdating {
            return lhs._autoupdating == rhs._autoupdating
        } else {
            return lhs._wrapped.isEqual(rhs._wrapped)
        }
    }
}

extension Locale : CustomDebugStringConvertible, CustomStringConvertible, CustomReflectable {
    private var _kindDescription : String {
        if self == Locale.autoupdatingCurrent {
            return "autoupdatingCurrent"
        } else if self == Locale.current {
            return "current"
        } else {
            return "fixed"
        }
    }
    
    public var customMirror : Mirror {
        var c: [(label: String?, value: Any)] = []
        c.append((label: "identifier", value: identifier))
        c.append((label: "kind", value: _kindDescription))
        return Mirror(self, children: c, displayStyle: Mirror.DisplayStyle.struct)
    }
    
    public var description: String {
        return "\(identifier) (\(_kindDescription))"
    }
    
    public var debugDescription : String {
        return "\(identifier) (\(_kindDescription))"
    }
}

extension Locale : _ObjectiveCBridgeable {
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSLocale {
        return _wrapped
    }
    
    public static func _forceBridgeFromObjectiveC(_ input: NSLocale, result: inout Locale?) {
        if !_conditionallyBridgeFromObjectiveC(input, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ input: NSLocale, result: inout Locale?) -> Bool {
        result = Locale(reference: input)
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSLocale?) -> Locale {
        var result: Locale?
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension NSLocale : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as Locale)
    }
}

