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

/**
 `DateComponents` encapsulates the components of a date in an extendable, structured manner. 
 
 It is used to specify a date by providing the temporal components that make up a date and time in a particular calendar: hour, minutes, seconds, day, month, year, and so on. It can also be used to specify a duration of time, for example, 5 hours and 16 minutes. A `DateComponents` is not required to define all the component fields. 
 
 When a new instance of `DateComponents` is created, the date components are set to `nil`.
*/
public struct DateComponents : ReferenceConvertible, Hashable, Equatable, _MutableBoxing {
    public typealias ReferenceType = NSDateComponents
    
    internal var _handle: _MutableHandle<NSDateComponents>

    /// Initialize a `DateComponents`, optionally specifying values for its fields.
    public init(calendar: Calendar? = nil,
         timeZone: TimeZone? = nil,
         era: Int? = nil,
         year: Int? = nil,
         month: Int? = nil,
         day: Int? = nil,
         hour: Int? = nil,
         minute: Int? = nil,
         second: Int? = nil,
         nanosecond: Int? = nil,
         weekday: Int? = nil,
         weekdayOrdinal: Int? = nil,
         quarter: Int? = nil,
         weekOfMonth: Int? = nil,
         weekOfYear: Int? = nil,
         yearForWeekOfYear: Int? = nil) {
        _handle = _MutableHandle(adoptingReference: NSDateComponents())
        if let _calendar = calendar { self.calendar = _calendar }
        if let _timeZone = timeZone { self.timeZone = _timeZone }
        if let _era = era { self.era = _era }
        if let _year = year { self.year = _year }
        if let _month = month { self.month = _month }
        if let _day = day { self.day = _day }
        if let _hour = hour { self.hour = _hour }
        if let _minute = minute { self.minute = _minute }
        if let _second = second { self.second = _second }
        if let _nanosecond = nanosecond { self.nanosecond = _nanosecond }
        if let _weekday = weekday { self.weekday = _weekday }
        if let _weekdayOrdinal = weekdayOrdinal { self.weekdayOrdinal = _weekdayOrdinal }
        if let _quarter = quarter { self.quarter = _quarter }
        if let _weekOfMonth = weekOfMonth { self.weekOfMonth = _weekOfMonth }
        if let _weekOfYear = weekOfYear { self.weekOfYear = _weekOfYear }
        if let _yearForWeekOfYear = yearForWeekOfYear { self.yearForWeekOfYear = _yearForWeekOfYear }
    }

    
    // MARK: - Properties
    
    /// Translate from the NSDateComponentUndefined value into a proper Swift optional
    private func _getter(_ x : Int) -> Int? { return x == NSDateComponentUndefined ? nil : x }
    
    /// Translate from the proper Swift optional value into an NSDateComponentUndefined
    private func _setter(_ x : Int?) -> Int { if let xx = x { return xx } else { return NSDateComponentUndefined } }

    /// The `Calendar` used to interpret the other values in this structure.
    ///
    /// - note: API which uses `DateComponents` may have different behavior if this value is `nil`. For example, assuming the current calendar or ignoring certain values.
    public var calendar: Calendar? {
        get { return _handle.map { $0.calendar } }
        set { _applyMutation { $0.calendar = newValue } }
    }
    
    /// A time zone.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var timeZone: TimeZone? {
        get { return _handle.map { $0.timeZone } }
        set { _applyMutation { $0.timeZone = newValue } }
    }
    
    /// An era or count of eras.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var era: Int? {
        get { return _handle.map { _getter($0.era) } }
        set { _applyMutation { $0.era = _setter(newValue) } }
    }
    
    /// A year or count of years.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var year: Int? {
        get { return _handle.map { _getter($0.year) } }
        set { _applyMutation { $0.year = _setter(newValue) } }
    }
    
    /// A month or count of months.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var month: Int? {
        get { return _handle.map { _getter($0.month) } }
        set { _applyMutation { $0.month = _setter(newValue) } }
    }
    
    /// A day or count of days.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var day: Int? {
        get { return _handle.map { _getter($0.day) } }
        set { _applyMutation { $0.day = _setter(newValue) } }
    }
    
    /// An hour or count of hours.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var hour: Int? {
        get { return _handle.map { _getter($0.hour) } }
        set { _applyMutation { $0.hour = _setter(newValue) } }
    }
    
    /// A minute or count of minutes.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var minute: Int? {
        get { return _handle.map { _getter($0.minute) } }
        set { _applyMutation { $0.minute = _setter(newValue) } }
    }
    
    /// A second or count of seconds.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var second: Int? {
        get { return _handle.map { _getter($0.second) } }
        set { _applyMutation { $0.second = _setter(newValue) } }
    }
    
    /// A nanosecond or count of nanoseconds.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var nanosecond: Int? {
        get { return _handle.map { _getter($0.nanosecond) } }
        set { _applyMutation { $0.nanosecond = _setter(newValue) } }
    }
    
    /// A weekday or count of weekdays.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var weekday: Int? {
        get { return _handle.map { _getter($0.weekday) } }
        set { _applyMutation { $0.weekday = _setter(newValue) } }
    }
    
    /// A weekday ordinal or count of weekday ordinals.
    /// Weekday ordinal units represent the position of the weekday within the next larger calendar unit, such as the month. For example, 2 is the weekday ordinal unit for the second Friday of the month.///
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var weekdayOrdinal: Int? {
        get { return _handle.map { _getter($0.weekdayOrdinal) } }
        set { _applyMutation { $0.weekdayOrdinal = _setter(newValue) } }
    }
    
    /// A quarter or count of quarters.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var quarter: Int? {
        get { return _handle.map { _getter($0.quarter) } }
        set { _applyMutation { $0.quarter = _setter(newValue) } }
    }
    
    /// A week of the month or a count of weeks of the month.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var weekOfMonth: Int? {
        get { return _handle.map { _getter($0.weekOfMonth) } }
        set { _applyMutation { $0.weekOfMonth = _setter(newValue) } }
    }
    
    /// A week of the year or count of the weeks of the year.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var weekOfYear: Int? {
        get { return _handle.map { _getter($0.weekOfYear) } }
        set { _applyMutation { $0.weekOfYear = _setter(newValue) } }
    }
    
    /// The ISO 8601 week-numbering year of the receiver.
    ///
    /// The Gregorian calendar defines a week to have 7 days, and a year to have 356 days, or 366 in a leap year. However, neither 356 or 366 divide evenly into a 7 day week, so it is often the case that the last week of a year ends on a day in the next year, and the first week of a year begins in the preceding year. To reconcile this, ISO 8601 defines a week-numbering year, consisting of either 52 or 53 full weeks (364 or 371 days), such that the first week of a year is designated to be the week containing the first Thursday of the year.
    ///
    /// You can use the yearForWeekOfYear property with the weekOfYear and weekday properties to get the date corresponding to a particular weekday of a given week of a year. For example, the 6th day of the 53rd week of the year 2005 (ISO 2005-W53-6) corresponds to Sat 1 January 2005 on the Gregorian calendar.
    /// - note: This value is interpreted in the context of the calendar in which it is used.
    public var yearForWeekOfYear: Int? {
        get { return _handle.map { _getter($0.yearForWeekOfYear) } }
        set { _applyMutation { $0.yearForWeekOfYear = _setter(newValue) } }
    }
    
    /// Set to true if these components represent a leap month.
    public var isLeapMonth: Bool? {
        get { return _handle.map { $0.isLeapMonth } }
        set {
            _applyMutation {
                // Technically, the underlying class does not support setting isLeapMonth to nil, but it could - so we leave the API consistent.
                if let b = newValue {
                    $0.isLeapMonth = b
                } else {
                    $0.isLeapMonth = false
                }
            }
        }
    }
    
    /// Returns a `Date` calculated from the current components using the `calendar` property.
    public var date: Date? {
        if let d = _handle.map({$0.date}) {
            return d as Date
        } else {
            return nil
        }
    }
    
    // MARK: - Generic Setter/Getters
    
    /// Set the value of one of the properties, using an enumeration value instead of a property name.
    ///
    /// The calendar and timeZone and isLeapMonth properties cannot be set by this method.
    @available(OSX 10.9, iOS 8.0, *)
    public mutating func setValue(_ value: Int?, for component: Calendar.Component) {
        _applyMutation {
            $0.setValue(_setter(value), forComponent: Calendar._toCalendarUnit([component]))
        }
    }
    
    /// Returns the value of one of the properties, using an enumeration value instead of a property name.
    ///
    /// The calendar and timeZone and isLeapMonth property values cannot be retrieved by this method.
    @available(OSX 10.9, iOS 8.0, *)
    public func value(for component: Calendar.Component) -> Int? {
        return _handle.map {
            $0.value(forComponent: Calendar._toCalendarUnit([component]))
        }
    }
    
    // MARK: -
    
    /// Returns true if the combination of properties which have been set in the receiver is a date which exists in the `calendar` property.
    ///
    /// This method is not appropriate for use on `DateComponents` values which are specifying relative quantities of calendar components.
    ///
    /// Except for some trivial cases (e.g., 'seconds' should be 0 - 59 in any calendar), this method is not necessarily cheap.
    ///
    /// If the time zone property is set in the `DateComponents`, it is used.
    ///
    /// The calendar property must be set, or the result is always `false`.
    @available(OSX 10.9, iOS 8.0, *)
    public var isValidDate: Bool {
        return _handle.map { $0.isValidDate }
    }
    
    /// Returns true if the combination of properties which have been set in the receiver is a date which exists in the specified `Calendar`.
    ///
    /// This method is not appropriate for use on `DateComponents` values which are specifying relative quantities of calendar components.
    ///
    /// Except for some trivial cases (e.g., 'seconds' should be 0 - 59 in any calendar), this method is not necessarily cheap.
    ///
    /// If the time zone property is set in the `DateComponents`, it is used.
    @available(OSX 10.9, iOS 8.0, *)
    public func isValidDate(in calendar: Calendar) -> Bool {
        return _handle.map { $0.isValidDate(in: calendar) }
    }
    
    // MARK: -
    
    public var hashValue : Int {
        return _handle.map { $0.hash }
    }
    
    // MARK: - Bridging Helpers
    
    fileprivate init(reference: NSDateComponents) {
        _handle = _MutableHandle(reference: reference)
    }

    public static func ==(lhs : DateComponents, rhs: DateComponents) -> Bool {
        // Don't copy references here; no one should be storing anything
        return lhs._handle._uncopiedReference().isEqual(rhs._handle._uncopiedReference())
    }

}

extension DateComponents : CustomStringConvertible, CustomDebugStringConvertible, CustomReflectable {
    
    public var description: String {
        return self.customMirror.children.reduce("") {
            $0.appending("\($1.label ?? ""): \($1.value) ")
        }
    }
    
    public var debugDescription: String {
        return self.description
    }
    
    public var customMirror: Mirror {
        var c: [(label: String?, value: Any)] = []
        if let r = calendar { c.append((label: "calendar", value: r)) }
        if let r = timeZone { c.append((label: "timeZone", value: r)) }
        if let r = era { c.append((label: "era", value: r)) }
        if let r = year { c.append((label: "year", value: r)) }
        if let r = month { c.append((label: "month", value: r)) }
        if let r = day { c.append((label: "day", value: r)) }
        if let r = hour { c.append((label: "hour", value: r)) }
        if let r = minute { c.append((label: "minute", value: r)) }
        if let r = second { c.append((label: "second", value: r)) }
        if let r = nanosecond { c.append((label: "nanosecond", value: r)) }
        if let r = weekday { c.append((label: "weekday", value: r)) }
        if let r = weekdayOrdinal { c.append((label: "weekdayOrdinal", value: r)) }
        if let r = quarter { c.append((label: "quarter", value: r)) }
        if let r = weekOfMonth { c.append((label: "weekOfMonth", value: r)) }
        if let r = weekOfYear { c.append((label: "weekOfYear", value: r)) }
        if let r = yearForWeekOfYear { c.append((label: "yearForWeekOfYear", value: r)) }
        if let r = isLeapMonth { c.append((label: "isLeapMonth", value: r)) }
        return Mirror(self, children: c, displayStyle: Mirror.DisplayStyle.struct)
    }
}

// MARK: - Bridging

extension DateComponents : _ObjectiveCBridgeable {
    public static func _getObjectiveCType() -> Any.Type {
        return NSDateComponents.self
    }
    
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSDateComponents {
        return _handle._copiedReference()
    }

    public static func _forceBridgeFromObjectiveC(_ dateComponents: NSDateComponents, result: inout DateComponents?) {
        if !_conditionallyBridgeFromObjectiveC(dateComponents, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ dateComponents: NSDateComponents, result: inout DateComponents?) -> Bool {
        result = DateComponents(reference: dateComponents)
        return true
    }

    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSDateComponents?) -> DateComponents {
        var result: DateComponents?
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension NSDateComponents : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as DateComponents)
    }
}

