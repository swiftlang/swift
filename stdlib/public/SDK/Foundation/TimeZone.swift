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
 `TimeZone` defines the behavior of a time zone. Time zone values represent geopolitical regions. Consequently, these values have names for these regions. Time zone values also represent a temporal offset, either plus or minus, from Greenwich Mean Time (GMT) and an abbreviation (such as PST for Pacific Standard Time).
 
 `TimeZone` provides two static functions to get time zone values: `current` and `autoupdatingCurrent`. The `autoupdatingCurrent` time zone automatically tracks updates made by the user.
 
 Note that time zone database entries such as "America/Los_Angeles" are IDs, not names. An example of a time zone name is "Pacific Daylight Time". Although many `TimeZone` functions include the word "name", they refer to IDs.
 
 Cocoa does not provide any API to change the time zone of the computer, or of other applications.
 */
public struct TimeZone : Hashable, Equatable, ReferenceConvertible {
    public typealias ReferenceType = NSTimeZone
    
    fileprivate var _wrapped : NSTimeZone
    private var _autoupdating : Bool
    
    /// The time zone currently used by the system.
    public static var current : TimeZone {
        return TimeZone(adoptingReference: __NSTimeZoneCurrent() as! NSTimeZone, autoupdating: false)
    }
    
    /// The time zone currently used by the system, automatically updating to the user's current preference.
    ///
    /// If this time zone is mutated, then it no longer tracks the system time zone.
    ///
    /// The autoupdating time zone only compares equal to itself.
    public static var autoupdatingCurrent : TimeZone {
        return TimeZone(adoptingReference: __NSTimeZoneAutoupdating() as! NSTimeZone, autoupdating: true)
    }
    
    // MARK: -
    //
    
    /// Returns a time zone initialized with a given identifier.
    ///
    /// An example identifier is "America/Los_Angeles".
    ///
    /// If `identifier` is an unknown identifier, then returns `nil`.
    public init?(identifier: __shared String) {
        if let r = NSTimeZone(name: identifier) {
            _wrapped = r
            _autoupdating = false
        } else {
            return nil
        }
    }
    
    @available(*, unavailable, renamed: "init(secondsFromGMT:)")
    public init(forSecondsFromGMT seconds: Int) { fatalError() }
    
    /// Returns a time zone initialized with a specific number of seconds from GMT.
    ///
    /// Time zones created with this never have daylight savings and the offset is constant no matter the date. The identifier and abbreviation do NOT follow the POSIX convention (of minutes-west).
    ///
    /// - parameter seconds: The number of seconds from GMT.
    /// - returns: A time zone, or `nil` if a valid time zone could not be created from `seconds`.
    public init?(secondsFromGMT seconds: Int) {
        if let r = NSTimeZone(forSecondsFromGMT: seconds) as NSTimeZone? {
            _wrapped = r
            _autoupdating = false
        } else {
            return nil
        }
    }
    
    /// Returns a time zone identified by a given abbreviation.
    ///
    /// In general, you are discouraged from using abbreviations except for unique instances such as "GMT". Time Zone abbreviations are not standardized and so a given abbreviation may have multiple meanings--for example, "EST" refers to Eastern Time in both the United States and Australia
    ///
    /// - parameter abbreviation: The abbreviation for the time zone.
    /// - returns: A time zone identified by abbreviation determined by resolving the abbreviation to an identifier using the abbreviation dictionary and then returning the time zone for that identifier. Returns `nil` if there is no match for abbreviation.
    public init?(abbreviation: __shared String) {
        if let r = NSTimeZone(abbreviation: abbreviation) {
            _wrapped = r
            _autoupdating = false
        } else {
            return nil
        }
    }
    
    fileprivate init(reference: NSTimeZone) {
        if __NSTimeZoneIsAutoupdating(reference) {
            // we can't copy this or we lose its auto-ness (27048257)
            // fortunately it's immutable
            _autoupdating = true
            _wrapped = reference
        } else {
            _autoupdating = false
            _wrapped = reference.copy() as! NSTimeZone
        }
    }

    private init(adoptingReference reference: NSTimeZone, autoupdating: Bool) {
        // this path is only used for types we do not need to copy (we are adopting the ref)
        _wrapped = reference
        _autoupdating = autoupdating
    }

    // MARK: -
    //
    
    @available(*, unavailable, renamed: "identifier")
    public var name: String { fatalError() }

    /// The geopolitical region identifier that identifies the time zone.
    public var identifier: String {
        return _wrapped.name
    }
    
    @available(*, unavailable, message: "use the identifier instead")
    public var data: Data { fatalError() }
    
    /// The current difference in seconds between the time zone and Greenwich Mean Time.
    ///
    /// - parameter date: The date to use for the calculation. The default value is the current date.
    public func secondsFromGMT(for date: Date = Date()) -> Int {
        return _wrapped.secondsFromGMT(for: date)
    }
    
    /// Returns the abbreviation for the time zone at a given date.
    ///
    /// Note that the abbreviation may be different at different dates. For example, during daylight saving time the US/Eastern time zone has an abbreviation of "EDT." At other times, its abbreviation is "EST."
    /// - parameter date: The date to use for the calculation. The default value is the current date.
    public func abbreviation(for date: Date = Date()) -> String? {
        return _wrapped.abbreviation(for: date)
    }
    
    /// Returns a Boolean value that indicates whether the receiver uses daylight saving time at a given date.
    ///
    /// - parameter date: The date to use for the calculation. The default value is the current date.
    public func isDaylightSavingTime(for date: Date = Date()) -> Bool {
        return _wrapped.isDaylightSavingTime(for: date)
    }
    
    /// Returns the daylight saving time offset for a given date.
    ///
    /// - parameter date: The date to use for the calculation. The default value is the current date.
    public func daylightSavingTimeOffset(for date: Date = Date()) -> TimeInterval {
        return _wrapped.daylightSavingTimeOffset(for: date)
    }
    
    /// Returns the next daylight saving time transition after a given date.
    ///
    /// - parameter date: A date.
    /// - returns: The next daylight saving time transition after `date`. Depending on the time zone, this function may return a change of the time zone's offset from GMT. Returns `nil` if the time zone of the receiver does not observe daylight savings time as of `date`.
    public func nextDaylightSavingTimeTransition(after date: Date) -> Date? {
        return _wrapped.nextDaylightSavingTimeTransition(after: date)
    }
    
    /// Returns an array of strings listing the identifier of all the time zones known to the system.
    public static var knownTimeZoneIdentifiers : [String] {
        return NSTimeZone.knownTimeZoneNames
    }
    
    /// Returns the mapping of abbreviations to time zone identifiers.
    public static var abbreviationDictionary : [String : String] {
        get {
            return NSTimeZone.abbreviationDictionary
        }
        set {
            NSTimeZone.abbreviationDictionary = newValue
        }
    }
    
    /// Returns the time zone data version.
    public static var timeZoneDataVersion : String {
        return NSTimeZone.timeZoneDataVersion
    }
    
    /// Returns the date of the next (after the current instant) daylight saving time transition for the time zone. Depending on the time zone, the value of this property may represent a change of the time zone's offset from GMT. Returns `nil` if the time zone does not currently observe daylight saving time.
    public var nextDaylightSavingTimeTransition: Date? {
        return _wrapped.nextDaylightSavingTimeTransition
    }

    @available(*, unavailable, renamed: "localizedName(for:locale:)")
    public func localizedName(_ style: NSTimeZone.NameStyle, locale: Locale?) -> String? { fatalError() }

    /// Returns the name of the receiver localized for a given locale.
    public func localizedName(for style: NSTimeZone.NameStyle, locale: Locale?) -> String? {
        return _wrapped.localizedName(style, locale: locale)
    }
    
    // MARK: -
    
    public var hashValue : Int { // FIXME(hashValue): Remove
        if _autoupdating {
            return 1
        } else {
            return _wrapped.hash
        }
    }

    public func hash(into hasher: inout Hasher) {
        if _autoupdating {
            hasher.combine(false)
        } else {
            hasher.combine(true)
            hasher.combine(_wrapped)
        }
    }

    public static func ==(lhs: TimeZone, rhs: TimeZone) -> Bool {
        if lhs._autoupdating || rhs._autoupdating {
            return lhs._autoupdating == rhs._autoupdating
        } else {
            return lhs._wrapped.isEqual(rhs._wrapped)
        }
    }
}

extension TimeZone : CustomStringConvertible, CustomDebugStringConvertible, CustomReflectable {
    private var _kindDescription : String {
        if self == TimeZone.autoupdatingCurrent {
            return "autoupdatingCurrent"
        } else if self == TimeZone.current {
            return "current"
        } else {
            return "fixed"
        }
    }
    
    public var customMirror : Mirror {
        let c: [(label: String?, value: Any)] = [
          ("identifier", identifier),
          ("kind", _kindDescription),
          ("abbreviation", abbreviation() as Any),
          ("secondsFromGMT", secondsFromGMT()),
          ("isDaylightSavingTime", isDaylightSavingTime()),
        ]
        return Mirror(self, children: c, displayStyle: Mirror.DisplayStyle.struct)
    }
    
    public var description: String {
        return "\(identifier) (\(_kindDescription))"
    }
    
    public var debugDescription : String {
        return "\(identifier) (\(_kindDescription))"
    }
}

extension TimeZone : _ObjectiveCBridgeable {
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSTimeZone {
        // _wrapped is immutable
        return _wrapped
    }
    
    public static func _forceBridgeFromObjectiveC(_ input: NSTimeZone, result: inout TimeZone?) {
        if !_conditionallyBridgeFromObjectiveC(input, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ input: NSTimeZone, result: inout TimeZone?) -> Bool {
        result = TimeZone(reference: input)
        return true
    }
    
    @_effects(readonly)
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSTimeZone?) -> TimeZone {
        var result: TimeZone?
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension NSTimeZone : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as TimeZone)
    }
}

extension TimeZone : Codable {
    private enum CodingKeys : Int, CodingKey {
        case identifier
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let identifier = try container.decode(String.self, forKey: .identifier)

        guard let timeZone = TimeZone(identifier: identifier) else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath,
                                                                    debugDescription: "Invalid TimeZone identifier."))
        }

        self = timeZone
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(self.identifier, forKey: .identifier)
    }
}
