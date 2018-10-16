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
 `Calendar` encapsulates information about systems of reckoning time in which the beginning, length, and divisions of a year are defined. It provides information about the calendar and support for calendrical computations such as determining the range of a given calendrical unit and adding units to a given absolute time.
*/
public struct Calendar : Hashable, Equatable, ReferenceConvertible, _MutableBoxing {
    public typealias ReferenceType = NSCalendar
    
    private var _autoupdating: Bool
    internal var _handle: _MutableHandle<NSCalendar>

    /// Calendar supports many different kinds of calendars. Each is identified by an identifier here.
    public enum Identifier {
        /// The common calendar in Europe, the Western Hemisphere, and elsewhere.
        case gregorian
        
        case buddhist
        case chinese
        case coptic
        case ethiopicAmeteMihret
        case ethiopicAmeteAlem
        case hebrew
        case iso8601
        case indian
        case islamic
        case islamicCivil
        case japanese
        case persian
        case republicOfChina
        
        /// A simple tabular Islamic calendar using the astronomical/Thursday epoch of CE 622 July 15
        @available(macOS 10.10, iOS 8.0, *)
        case islamicTabular
        
        /// The Islamic Umm al-Qura calendar used in Saudi Arabia. This is based on astronomical calculation, instead of tabular behavior.
        @available(macOS 10.10, iOS 8.0, *)
        case islamicUmmAlQura
        
    }
    
    /// An enumeration for the various components of a calendar date.
    ///
    /// Several `Calendar` APIs use either a single unit or a set of units as input to a search algorithm.
    ///
    /// - seealso: `DateComponents`
    public enum Component {
        case era
        case year
        case month
        case day
        case hour
        case minute
        case second
        case weekday
        case weekdayOrdinal
        case quarter
        case weekOfMonth
        case weekOfYear
        case yearForWeekOfYear
        case nanosecond
        case calendar
        case timeZone
    }
    
    /// Returns the user's current calendar.
    ///
    /// This calendar does not track changes that the user makes to their preferences.
    public static var current : Calendar {
        return Calendar(adoptingReference: __NSCalendarCurrent() as! NSCalendar, autoupdating: false)
    }
    
    /// A Calendar that tracks changes to user's preferred calendar.
    ///
    /// If mutated, this calendar will no longer track the user's preferred calendar.
    ///
    /// - note: The autoupdating Calendar will only compare equal to another autoupdating Calendar.
    public static var autoupdatingCurrent : Calendar {
        return Calendar(adoptingReference: __NSCalendarAutoupdating() as! NSCalendar, autoupdating: true)
    }

    // MARK: -
    // MARK: init
    
    /// Returns a new Calendar.
    ///
    /// - parameter identifier: The kind of calendar to use.
    public init(identifier: __shared Identifier) {
        let result = __NSCalendarCreate(Calendar._toNSCalendarIdentifier(identifier))
        _handle = _MutableHandle(adoptingReference: result as! NSCalendar)
        _autoupdating = false
    }
    
    // MARK: -
    // MARK: Bridging
    
    fileprivate init(reference : __shared NSCalendar) {
        _handle = _MutableHandle(reference: reference)
        if __NSCalendarIsAutoupdating(reference) {
            _autoupdating = true
        } else {
            _autoupdating = false
        }
    }

    private init(adoptingReference reference: NSCalendar, autoupdating: Bool) {
        _handle = _MutableHandle(adoptingReference: reference)
        _autoupdating = autoupdating
    }

    // MARK: -
    // 
    
    /// The identifier of the calendar.
    public var identifier : Identifier {
        return Calendar._fromNSCalendarIdentifier(_handle.map({ $0.calendarIdentifier }))
    }

    /// The locale of the calendar.
    public var locale : Locale? {
        get {
            return _handle.map { $0.locale }
        }
        set {
            _applyMutation { $0.locale = newValue }
        }
    }
    
    /// The time zone of the calendar.
    public var timeZone : TimeZone {
        get {
            return _handle.map { $0.timeZone }
        }
        set {
            _applyMutation { $0.timeZone = newValue }
        }
    }
    
    /// The first weekday of the calendar.
    public var firstWeekday : Int {
        get {
            return _handle.map { $0.firstWeekday }
        }
        set {
            _applyMutation { $0.firstWeekday = newValue }
        }
    }
    
    /// The number of minimum days in the first week.
    public var minimumDaysInFirstWeek : Int {
        get {
            return _handle.map { $0.minimumDaysInFirstWeek }
        }
        set {
            _applyMutation { $0.minimumDaysInFirstWeek = newValue }
        }
    }
    
    /// A list of eras in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["BC", "AD"]`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var eraSymbols: [String] {
        return _handle.map { $0.eraSymbols }
    }
    
    /// A list of longer-named eras in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["Before Christ", "Anno Domini"]`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var longEraSymbols: [String] {
        return _handle.map { $0.longEraSymbols }
    }
    
    /// A list of months in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var monthSymbols: [String] {
        return _handle.map { $0.monthSymbols }
    }
    
    /// A list of shorter-named months in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var shortMonthSymbols: [String] {
        return _handle.map { $0.shortMonthSymbols }
    }
    
    /// A list of very-shortly-named months in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"]`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var veryShortMonthSymbols: [String] {
        return _handle.map { $0.veryShortMonthSymbols }
    }
    
    /// A list of standalone months in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]`.
    /// - note: Stand-alone properties are for use in places like calendar headers. Non-stand-alone properties are for use in context (for example, "Saturday, November 12th").
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var standaloneMonthSymbols: [String] {
        return _handle.map { $0.standaloneMonthSymbols }
    }
    
    /// A list of shorter-named standalone months in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]`.
    /// - note: Stand-alone properties are for use in places like calendar headers. Non-stand-alone properties are for use in context (for example, "Saturday, November 12th").
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var shortStandaloneMonthSymbols: [String] {
        return _handle.map { $0.shortStandaloneMonthSymbols }
    }
    
    /// A list of very-shortly-named standalone months in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"]`.
    /// - note: Stand-alone properties are for use in places like calendar headers. Non-stand-alone properties are for use in context (for example, "Saturday, November 12th").
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var veryShortStandaloneMonthSymbols: [String] {
        return _handle.map { $0.veryShortStandaloneMonthSymbols }
    }
    
    /// A list of weekdays in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var weekdaySymbols: [String] {
        return _handle.map { $0.weekdaySymbols }
    }
    
    /// A list of shorter-named weekdays in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var shortWeekdaySymbols: [String] {
        return _handle.map { $0.shortWeekdaySymbols }
    }
    
    /// A list of very-shortly-named weekdays in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["S", "M", "T", "W", "T", "F", "S"]`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var veryShortWeekdaySymbols: [String] {
        return _handle.map { $0.veryShortWeekdaySymbols }
    }
    
    /// A list of standalone weekday names in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]`.
    /// - note: Stand-alone properties are for use in places like calendar headers. Non-stand-alone properties are for use in context (for example, "Saturday, November 12th").
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var standaloneWeekdaySymbols: [String] {
        return _handle.map { $0.standaloneWeekdaySymbols }
    }
    
    /// A list of shorter-named standalone weekdays in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]`.
    /// - note: Stand-alone properties are for use in places like calendar headers. Non-stand-alone properties are for use in context (for example, "Saturday, November 12th").
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var shortStandaloneWeekdaySymbols: [String] {
        return _handle.map { $0.shortStandaloneWeekdaySymbols }
    }
    
    /// A list of very-shortly-named weekdays in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["S", "M", "T", "W", "T", "F", "S"]`.
    /// - note: Stand-alone properties are for use in places like calendar headers. Non-stand-alone properties are for use in context (for example, "Saturday, November 12th").
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var veryShortStandaloneWeekdaySymbols: [String] {
        return _handle.map { $0.veryShortStandaloneWeekdaySymbols }
    }
    
    /// A list of quarter names in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["1st quarter", "2nd quarter", "3rd quarter", "4th quarter"]`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var quarterSymbols: [String] {
        return _handle.map { $0.quarterSymbols }
    }
    
    /// A list of shorter-named quarters in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["Q1", "Q2", "Q3", "Q4"]`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var shortQuarterSymbols: [String] {
        return _handle.map { $0.shortQuarterSymbols }
    }
    
    /// A list of standalone quarter names in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["1st quarter", "2nd quarter", "3rd quarter", "4th quarter"]`.
    /// - note: Stand-alone properties are for use in places like calendar headers. Non-stand-alone properties are for use in context (for example, "Saturday, November 12th").
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var standaloneQuarterSymbols: [String] {
        return _handle.map { $0.standaloneQuarterSymbols }
    }
    
    /// A list of shorter-named standalone quarters in this calendar, localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `["Q1", "Q2", "Q3", "Q4"]`.
    /// - note: Stand-alone properties are for use in places like calendar headers. Non-stand-alone properties are for use in context (for example, "Saturday, November 12th").
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var shortStandaloneQuarterSymbols: [String] {
        return _handle.map { $0.shortStandaloneQuarterSymbols }
    }
    
    /// The symbol used to represent "AM", localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `"AM"`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var amSymbol: String {
        return _handle.map { $0.amSymbol }
    }
    
    /// The symbol used to represent "PM", localized to the Calendar's `locale`.
    ///
    /// For example, for English in the Gregorian calendar, returns `"PM"`.
    ///
    /// - note: By default, Calendars have no locale set. If you wish to receive a localized answer, be sure to set the `locale` property first - most likely to `Locale.autoupdatingCurrent`.
    public var pmSymbol: String {
        return _handle.map { $0.pmSymbol }
    }
    
    // MARK: -
    //
    
    /// Returns the minimum range limits of the values that a given component can take on in the receiver.
    ///
    /// As an example, in the Gregorian calendar the minimum range of values for the Day component is 1-28.
    /// - parameter component: A component to calculate a range for.
    /// - returns: The range, or nil if it could not be calculated.
    public func minimumRange(of component: Component) -> Range<Int>? {
        return _handle.map { $0.minimumRange(of: Calendar._toCalendarUnit([component])).toRange() }
    }
    
    /// The maximum range limits of the values that a given component can take on in the receive
    ///
    /// As an example, in the Gregorian calendar the maximum range of values for the Day component is 1-31.
    /// - parameter component: A component to calculate a range for.
    /// - returns: The range, or nil if it could not be calculated.
    public func maximumRange(of component: Component) -> Range<Int>? {
        return _handle.map { $0.maximumRange(of: Calendar._toCalendarUnit([component])).toRange() }
    }
    
    
    @available(*, unavailable, message: "use range(of:in:for:) instead")
    public func range(of smaller: NSCalendar.Unit, in larger: NSCalendar.Unit, for date: Date) -> NSRange {
        fatalError()
    }
    
    /// Returns the range of absolute time values that a smaller calendar component (such as a day) can take on in a larger calendar component (such as a month) that includes a specified absolute time.
    ///
    /// You can use this method to calculate, for example, the range the `day` component can take on in the `month` in which `date` lies.
    /// - parameter smaller: The smaller calendar component.
    /// - parameter larger: The larger calendar component.
    /// - parameter date: The absolute time for which the calculation is performed.
    /// - returns: The range of absolute time values smaller can take on in larger at the time specified by date. Returns `nil` if larger is not logically bigger than smaller in the calendar, or the given combination of components does not make sense (or is a computation which is undefined).
    public func range(of smaller: Component, in larger: Component, for date: Date) -> Range<Int>? {
        return _handle.map { $0.range(of: Calendar._toCalendarUnit([smaller]), in: Calendar._toCalendarUnit([larger]), for: date).toRange() }
    }
    
    @available(*, unavailable, message: "use range(of:in:for:) instead")
    public func range(of unit: NSCalendar.Unit, start datep: AutoreleasingUnsafeMutablePointer<NSDate?>?, interval tip: UnsafeMutablePointer<TimeInterval>?, for date: Date) -> Bool {
        fatalError()
    }
    
    /// Returns, via two inout parameters, the starting time and duration of a given calendar component that contains a given date.
    ///
    /// - seealso: `range(of:for:)`
    /// - seealso: `dateInterval(of:for:)`
    /// - parameter component: A calendar component.
    /// - parameter start: Upon return, the starting time of the calendar component that contains the date.
    /// - parameter interval: Upon return, the duration of the calendar component that contains the date.
    /// - parameter date: The specified date.
    /// - returns: `true` if the starting time and duration of a component could be calculated, otherwise `false`.
    public func dateInterval(of component: Component, start: inout Date, interval: inout TimeInterval, for date: Date) -> Bool {
        var nsDate : NSDate?
        var ti : TimeInterval = 0
        if _handle.map({ $0.range(of: Calendar._toCalendarUnit([component]), start: &nsDate, interval: &ti, for: date) }) {
            start = nsDate! as Date
            interval = ti
            return true
        } else {
            return false
        }
    }
    
    /// Returns the starting time and duration of a given calendar component that contains a given date.
    ///
    /// - parameter component: A calendar component.
    /// - parameter date: The specified date.
    /// - returns: A new `DateInterval` if the starting time and duration of a component could be calculated, otherwise `nil`.
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public func dateInterval(of component: Component, for date: Date) -> DateInterval? {
        var start : Date = Date(timeIntervalSinceReferenceDate: 0)
        var interval : TimeInterval = 0
        if self.dateInterval(of: component, start: &start, interval: &interval, for: date) {
            return DateInterval(start: start, duration: interval)
        } else {
            return nil
        }
    }
    
    /// Returns, for a given absolute time, the ordinal number of a smaller calendar component (such as a day) within a specified larger calendar component (such as a week).
    ///
    /// The ordinality is in most cases not the same as the decomposed value of the component. Typically return values are 1 and greater. For example, the time 00:45 is in the first hour of the day, and for components `hour` and `day` respectively, the result would be 1. An exception is the week-in-month calculation, which returns 0 for days before the first week in the month containing the date.
    ///
    /// - note: Some computations can take a relatively long time.
    /// - parameter smaller: The smaller calendar component.
    /// - parameter larger: The larger calendar component.
    /// - parameter date: The absolute time for which the calculation is performed.
    /// - returns: The ordinal number of smaller within larger at the time specified by date. Returns `nil` if larger is not logically bigger than smaller in the calendar, or the given combination of components does not make sense (or is a computation which is undefined).
    public func ordinality(of smaller: Component, in larger: Component, for date: Date) -> Int? {
        let result = _handle.map { $0.ordinality(of: Calendar._toCalendarUnit([smaller]), in: Calendar._toCalendarUnit([larger]), for: date) }
        if result == NSNotFound { return nil }
        return result
    }
    
    // MARK: -
    //
    
    @available(*, unavailable, message: "use dateComponents(_:from:) instead")
    public func getEra(_ eraValuePointer: UnsafeMutablePointer<Int>?, year yearValuePointer: UnsafeMutablePointer<Int>?, month monthValuePointer: UnsafeMutablePointer<Int>?, day dayValuePointer: UnsafeMutablePointer<Int>?, from date: Date) { fatalError() }
    
    @available(*, unavailable, message: "use dateComponents(_:from:) instead")
    public func getEra(_ eraValuePointer: UnsafeMutablePointer<Int>?, yearForWeekOfYear yearValuePointer: UnsafeMutablePointer<Int>?, weekOfYear weekValuePointer: UnsafeMutablePointer<Int>?, weekday weekdayValuePointer: UnsafeMutablePointer<Int>?, from date: Date) { fatalError() }
    
    @available(*, unavailable, message: "use dateComponents(_:from:) instead")
    public func getHour(_ hourValuePointer: UnsafeMutablePointer<Int>?, minute minuteValuePointer: UnsafeMutablePointer<Int>?, second secondValuePointer: UnsafeMutablePointer<Int>?, nanosecond nanosecondValuePointer: UnsafeMutablePointer<Int>?, from date: Date) { fatalError() }
    
    // MARK: -
    //
    
    
    @available(*, unavailable, message: "use date(byAdding:to:wrappingComponents:) instead")
    public func date(byAdding unit: NSCalendar.Unit, value: Int, to date: Date, options: NSCalendar.Options = []) -> Date? { fatalError() }
    
    /// Returns a new `Date` representing the date calculated by adding components to a given date.
    ///
    /// - parameter components: A set of values to add to the date.
    /// - parameter date: The starting date.
    /// - parameter wrappingComponents: If `true`, the component should be incremented and wrap around to zero/one on overflow, and should not cause higher components to be incremented. The default value is `false`.
    /// - returns: A new date, or nil if a date could not be calculated with the given input.
    public func date(byAdding components: DateComponents, to date: Date, wrappingComponents: Bool = false) -> Date? {
        return _handle.map { $0.date(byAdding: components, to: date, options: wrappingComponents ? [.wrapComponents] : []) }
    }
    
    
    @available(*, unavailable, message: "use date(byAdding:to:wrappingComponents:) instead")
    public func date(byAdding comps: DateComponents, to date: Date, options opts: NSCalendar.Options = []) -> Date? { fatalError() }
    
    /// Returns a new `Date` representing the date calculated by adding an amount of a specific component to a given date.
    ///
    /// - parameter component: A single component to add.
    /// - parameter value: The value of the specified component to add.
    /// - parameter date: The starting date.
    /// - parameter wrappingComponents: If `true`, the component should be incremented and wrap around to zero/one on overflow, and should not cause higher components to be incremented. The default value is `false`.
    /// - returns: A new date, or nil if a date could not be calculated with the given input.
    @available(iOS 8.0, *)
    public func date(byAdding component: Component, value: Int, to date: Date, wrappingComponents: Bool = false) -> Date? {
        return _handle.map { $0.date(byAdding: Calendar._toCalendarUnit([component]), value: value, to: date, options: wrappingComponents ? [.wrapComponents] : []) }
    }
    
    /// Returns a date created from the specified components.
    ///
    /// - parameter components: Used as input to the search algorithm for finding a corresponding date.
    /// - returns: A new `Date`, or nil if a date could not be found which matches the components.
    public func date(from components: DateComponents) -> Date? {
        return _handle.map { $0.date(from: components) }
    }
    
    @available(*, unavailable, renamed: "dateComponents(_:from:)")
    public func components(_ unitFlags: NSCalendar.Unit, from date: Date) -> DateComponents { fatalError() }
    
    /// Returns all the date components of a date, using the calendar time zone.
    ///
    /// - note: If you want "date information in a given time zone" in order to display it, you should use `DateFormatter` to format the date.
    /// - parameter date: The `Date` to use.
    /// - returns: The date components of the specified date.
    public func dateComponents(_ components: Set<Component>, from date: Date) -> DateComponents {
        return _handle.map { $0.components(Calendar._toCalendarUnit(components), from: date) }
    }
    
    
    @available(*, unavailable, renamed: "dateComponents(in:from:)")
    public func components(in timezone: TimeZone, from date: Date) -> DateComponents { fatalError() }
    
    /// Returns all the date components of a date, as if in a given time zone (instead of the `Calendar` time zone).
    ///
    /// The time zone overrides the time zone of the `Calendar` for the purposes of this calculation.
    /// - note: If you want "date information in a given time zone" in order to display it, you should use `DateFormatter` to format the date.
    /// - parameter timeZone: The `TimeZone` to use.
    /// - parameter date: The `Date` to use.
    /// - returns: All components, calculated using the `Calendar` and `TimeZone`.
    @available(iOS 8.0, *)
    public func dateComponents(in timeZone: TimeZone, from date: Date) -> DateComponents {
        return _handle.map { $0.components(in: timeZone, from: date) }
    }
    
    @available(*, unavailable, renamed: "dateComponents(_:from:to:)")
    public func components(_ unitFlags: NSCalendar.Unit, from startingDate: Date, to resultDate: Date, options opts: NSCalendar.Options = []) -> DateComponents { fatalError() }
    
    /// Returns the difference between two dates.
    ///
    /// - parameter components: Which components to compare.
    /// - parameter start: The starting date.
    /// - parameter end: The ending date.
    /// - returns: The result of calculating the difference from start to end.
    public func dateComponents(_ components: Set<Component>, from start: Date, to end: Date) -> DateComponents {
        return _handle.map { $0.components(Calendar._toCalendarUnit(components), from: start, to: end, options: []) }
    }
    
    @available(*, unavailable, renamed: "dateComponents(_:from:to:)")
    public func components(_ unitFlags: NSCalendar.Unit, from startingDateComp: DateComponents, to resultDateComp: DateComponents, options: NSCalendar.Options = []) -> DateComponents { fatalError() }
    
    /// Returns the difference between two dates specified as `DateComponents`.
    ///
    /// For components which are not specified in each `DateComponents`, but required to specify an absolute date, the base value of the component is assumed.  For example, for an `DateComponents` with just a `year` and a `month` specified, a `day` of 1, and an `hour`, `minute`, `second`, and `nanosecond` of 0 are assumed.
    /// Calendrical calculations with unspecified `year` or `year` value prior to the start of a calendar are not advised.
    /// For each `DateComponents`, if its `timeZone` property is set, that time zone is used for it. If the `calendar` property is set, that is used rather than the receiving calendar, and if both the `calendar` and `timeZone` are set, the `timeZone` property value overrides the time zone of the `calendar` property.
    ///
    /// - parameter components: Which components to compare.
    /// - parameter start: The starting date components.
    /// - parameter end: The ending date components.
    /// - returns: The result of calculating the difference from start to end.
    @available(iOS 8.0, *)
    public func dateComponents(_ components: Set<Component>, from start: DateComponents, to end: DateComponents) -> DateComponents {
        return _handle.map { $0.components(Calendar._toCalendarUnit(components), from: start, to: end, options: []) }
    }
    
    
    /// Returns the value for one component of a date.
    ///
    /// - parameter component: The component to calculate.
    /// - parameter date: The date to use.
    /// - returns: The value for the component.
    @available(iOS 8.0, *)
    public func component(_ component: Component, from date: Date) -> Int {
        return _handle.map { $0.component(Calendar._toCalendarUnit([component]), from: date) }
    }
    
    
    @available(*, unavailable, message: "Use date(from:) instead")
    public func date(era: Int, year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int, nanosecond: Int) -> Date? { fatalError() }
    
    
    @available(*, unavailable, message: "Use date(from:) instead")
    public func date(era: Int, yearForWeekOfYear: Int, weekOfYear: Int, weekday: Int, hour: Int, minute: Int, second: Int, nanosecond: Int) -> Date? { fatalError() }
    
    
    /// Returns the first moment of a given Date, as a Date.
    ///
    /// For example, pass in `Date()`, if you want the start of today.
    /// If there were two midnights, it returns the first.  If there was none, it returns the first moment that did exist.
    /// - parameter date: The date to search.
    /// - returns: The first moment of the given date.
    @available(iOS 8.0, *)
    public func startOfDay(for date: Date) -> Date {
        return _handle.map { $0.startOfDay(for: date) }
    }
    
    
    @available(*, unavailable, renamed: "compare(_:to:toGranularity:)")
    public func compare(_ date1: Date, to date2: Date, toUnitGranularity unit: NSCalendar.Unit) -> ComparisonResult { fatalError() }
    
    
    /// Compares the given dates down to the given component, reporting them `orderedSame` if they are the same in the given component and all larger components, otherwise either `orderedAscending` or `orderedDescending`.
    ///
    /// - parameter date1: A date to compare.
    /// - parameter date2: A date to compare.
    /// - parameter: component: A granularity to compare. For example, pass `.hour` to check if two dates are in the same hour.
    @available(iOS 8.0, *)
    public func compare(_ date1: Date, to date2: Date, toGranularity component: Component) -> ComparisonResult {
        return _handle.map { $0.compare(date1, to: date2, toUnitGranularity: Calendar._toCalendarUnit([component])) }
    }
    
    
    @available(*, unavailable, renamed: "isDate(_:equalTo:toGranularity:)")
    public func isDate(_ date1: Date, equalTo date2: Date, toUnitGranularity unit: NSCalendar.Unit) -> Bool { fatalError() }
    
    /// Compares the given dates down to the given component, reporting them equal if they are the same in the given component and all larger components.
    ///
    /// - parameter date1: A date to compare.
    /// - parameter date2: A date to compare.
    /// - parameter component: A granularity to compare. For example, pass `.hour` to check if two dates are in the same hour.
    /// - returns: `true` if the given date is within tomorrow.
    @available(iOS 8.0, *)
    public func isDate(_ date1: Date, equalTo date2: Date, toGranularity component: Component) -> Bool {
        return _handle.map { $0.isDate(date1, equalTo: date2, toUnitGranularity: Calendar._toCalendarUnit([component])) }
    }
    
    
    /// Returns `true` if the given date is within the same day as another date, as defined by the calendar and calendar's locale.
    ///
    /// - parameter date1: A date to check for containment.
    /// - parameter date2: A date to check for containment.
    /// - returns: `true` if `date1` and `date2` are in the same day.
    @available(iOS 8.0, *)
    public func isDate(_ date1: Date, inSameDayAs date2: Date) -> Bool {
        return _handle.map { $0.isDate(date1, inSameDayAs: date2) }
    }
    
    
    /// Returns `true` if the given date is within today, as defined by the calendar and calendar's locale.
    ///
    /// - parameter date: The specified date.
    /// - returns: `true` if the given date is within today.
    @available(iOS 8.0, *)
    public func isDateInToday(_ date: Date) -> Bool {
        return _handle.map { $0.isDateInToday(date) }
    }
    
    
    /// Returns `true` if the given date is within yesterday, as defined by the calendar and calendar's locale.
    ///
    /// - parameter date: The specified date.
    /// - returns: `true` if the given date is within yesterday.
    @available(iOS 8.0, *)
    public func isDateInYesterday(_ date: Date) -> Bool {
        return _handle.map { $0.isDateInYesterday(date) }
    }
    
    
    /// Returns `true` if the given date is within tomorrow, as defined by the calendar and calendar's locale.
    ///
    /// - parameter date: The specified date.
    /// - returns: `true` if the given date is within tomorrow.
    @available(iOS 8.0, *)
    public func isDateInTomorrow(_ date: Date) -> Bool {
        return _handle.map { $0.isDateInTomorrow(date) }
    }
    
    
    /// Returns `true` if the given date is within a weekend period, as defined by the calendar and calendar's locale.
    ///
    /// - parameter date: The specified date.
    /// - returns: `true` if the given date is within a weekend.
    @available(iOS 8.0, *)
    public func isDateInWeekend(_ date: Date) -> Bool {
        return _handle.map { $0.isDateInWeekend(date) }
    }
    
    @available(*, unavailable, message: "use dateIntervalOfWeekend(containing:start:interval:) instead")
    public func range(ofWeekendStart datep: AutoreleasingUnsafeMutablePointer<NSDate?>?, interval tip: UnsafeMutablePointer<TimeInterval>?, containing date: Date) -> Bool { fatalError() }
    
    /// Find the range of the weekend around the given date, returned via two by-reference parameters.
    ///
    /// Note that a given entire day within a calendar is not necessarily all in a weekend or not; weekends can start in the middle of a day in some calendars and locales.
    /// - seealso: `dateIntervalOfWeekend(containing:)`
    /// - parameter date: The date at which to start the search.
    /// - parameter start: When the result is `true`, set
    /// - returns: `true` if a date range could be found, and `false` if the date is not in a weekend.
    @available(iOS 8.0, *)
    public func dateIntervalOfWeekend(containing date: Date, start: inout Date, interval: inout TimeInterval) -> Bool {
        var nsDate : NSDate?
        var ti : TimeInterval = 0
        if _handle.map({ $0.range(ofWeekendStart: &nsDate, interval: &ti, containing: date) }) {
            start = nsDate! as Date
            interval = ti
            return true
        } else {
            return false
        }
    }
    
    /// Returns a `DateInterval` of the weekend contained by the given date, or nil if the date is not in a weekend.
    ///
    /// - parameter date: The date contained in the weekend.
    /// - returns: A `DateInterval`, or nil if the date is not in a weekend.
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public func dateIntervalOfWeekend(containing date: Date) -> DateInterval? {
        var nsDate : NSDate?
        var ti : TimeInterval = 0
        if _handle.map({ $0.range(ofWeekendStart: &nsDate, interval: &ti, containing: date) }) {
            return DateInterval(start: nsDate! as Date, duration: ti)
        } else {
            return nil
        }
    }
    
    
    @available(*, unavailable, message: "use nextWeekend(startingAfter:start:interval:direction:) instead")
    public func nextWeekendStart(_ datep: AutoreleasingUnsafeMutablePointer<NSDate?>?, interval tip: UnsafeMutablePointer<TimeInterval>?, options: NSCalendar.Options = [], after date: Date) -> Bool { fatalError() }
    
    /// Returns the range of the next weekend via two inout parameters. The weekend starts strictly after the given date.
    ///
    /// If `direction` is `.backward`, then finds the previous weekend range strictly before the given date.
    ///
    /// Note that a given entire Day within a calendar is not necessarily all in a weekend or not; weekends can start in the middle of a day in some calendars and locales.
    /// - parameter date: The date at which to begin the search.
    /// - parameter direction: Which direction in time to search. The default value is `.forward`.
    /// - returns: A `DateInterval`, or nil if the weekend could not be found.
    @available(iOS 8.0, *)
    public func nextWeekend(startingAfter date: Date, start: inout Date, interval: inout TimeInterval, direction: SearchDirection = .forward) -> Bool {
        // The implementation actually overrides previousKeepSmaller and nextKeepSmaller with matchNext, always - but strict still trumps all.
        var nsDate : NSDate?
        var ti : TimeInterval = 0
        if _handle.map({ $0.nextWeekendStart(&nsDate, interval: &ti, options: direction == .backward ? [.searchBackwards] : [], after: date) }) {
            start = nsDate! as Date
            interval = ti
            return true
        } else {
            return false
        }
    }
    
    /// Returns a `DateInterval` of the next weekend, which starts strictly after the given date.
    ///
    /// If `direction` is `.backward`, then finds the previous weekend range strictly before the given date.
    ///
    /// Note that a given entire Day within a calendar is not necessarily all in a weekend or not; weekends can start in the middle of a day in some calendars and locales.
    /// - parameter date: The date at which to begin the search.
    /// - parameter direction: Which direction in time to search. The default value is `.forward`.
    /// - returns: A `DateInterval`, or nil if weekends do not exist in the specific calendar or locale.
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public func nextWeekend(startingAfter date: Date, direction: SearchDirection = .forward) -> DateInterval? {
        // The implementation actually overrides previousKeepSmaller and nextKeepSmaller with matchNext, always - but strict still trumps all.
        var nsDate : NSDate?
        var ti : TimeInterval = 0
        if _handle.map({ $0.nextWeekendStart(&nsDate, interval: &ti, options: direction == .backward ? [.searchBackwards] : [], after: date) }) {
            /// WARNING: searching backwards is totally broken! 26643365
            return DateInterval(start: nsDate! as Date, duration: ti)
        } else {
            return nil
        }
    }
    
    // MARK: -
    // MARK: Searching
    
    /// The direction in time to search.
    public enum SearchDirection {
        /// Search for a date later in time than the start date.
        case forward
        
        /// Search for a date earlier in time than the start date.
        case backward
    }
    
    /// Determines which result to use when a time is repeated on a day in a calendar (for example, during a daylight saving transition when the times between 2:00am and 3:00am may happen twice).
    public enum RepeatedTimePolicy {
        /// If there are two or more matching times (all the components are the same, including isLeapMonth) before the end of the next instance of the next higher component to the highest specified component, then the algorithm will return the first occurrence.
        case first
        
        /// If there are two or more matching times (all the components are the same, including isLeapMonth) before the end of the next instance of the next higher component to the highest specified component, then the algorithm will return the last occurrence.
        case last
    }
    
    /// A hint to the search algorithm to control the method used for searching for dates.
    public enum MatchingPolicy {
        /// If there is no matching time before the end of the next instance of the next higher component to the highest specified component in the `DateComponents` argument, the algorithm will return the next existing time which exists.
        ///
        /// For example, during a daylight saving transition there may be no 2:37am. The result would then be 3:00am, if that does exist.
        case nextTime
        
        /// If specified, and there is no matching time before the end of the next instance of the next higher component to the highest specified component in the `DateComponents` argument, the method will return the next existing value of the missing component and preserves the lower components' values (e.g., no 2:37am results in 3:37am, if that exists).
        case nextTimePreservingSmallerComponents
        
        /// If there is no matching time before the end of the next instance of the next higher component to the highest specified component in the `DateComponents` argument, the algorithm will return the previous existing value of the missing component and preserves the lower components' values.
        ///
        /// For example, during a daylight saving transition there may be no 2:37am. The result would then be 1:37am, if that does exist.
        case previousTimePreservingSmallerComponents
        
        /// If specified, the algorithm travels as far forward or backward as necessary looking for a match.
        ///
        /// For example, if searching for Feb 29 in the Gregorian calendar, the algorithm may choose March 1 instead (for example, if the year is not a leap year). If you wish to find the next Feb 29 without the algorithm adjusting the next higher component in the specified `DateComponents`, then use the `strict` option.
        /// - note: There are ultimately implementation-defined limits in how far distant the search will go.
        case strict
    }
    
    @available(*, unavailable, message: "use nextWeekend(startingAfter:matching:matchingPolicy:repeatedTimePolicy:direction:using:) instead")
    public func enumerateDates(startingAfter start: Date, matching comps: DateComponents, options opts: NSCalendar.Options = [], using block: (Date?, Bool, UnsafeMutablePointer<ObjCBool>) -> Swift.Void) { fatalError() }
    
    /// Computes the dates which match (or most closely match) a given set of components, and calls the closure once for each of them, until the enumeration is stopped.
    ///
    /// There will be at least one intervening date which does not match all the components (or the given date itself must not match) between the given date and any result.
    ///
    /// If `direction` is set to `.backward`, this method finds the previous match before the given date. The intent is that the same matches as for a `.forward` search will be found (that is, if you are enumerating forwards or backwards for each hour with minute "27", the seconds in the date you will get in forwards search would obviously be 00, and the same will be true in a backwards search in order to implement this rule.  Similarly for DST backwards jumps which repeats times, you'll get the first match by default, where "first" is defined from the point of view of searching forwards.  So, when searching backwards looking for a particular hour, with no minute and second specified, you don't get a minute and second of 59:59 for the matching hour (which would be the nominal first match within a given hour, given the other rules here, when searching backwards).
    ///
    /// If an exact match is not possible, and requested with the `strict` option, nil is passed to the closure and the enumeration ends.  (Logically, since an exact match searches indefinitely into the future, if no match is found there's no point in continuing the enumeration.)
    ///
    /// Result dates have an integer number of seconds (as if 0 was specified for the nanoseconds property of the `DateComponents` matching parameter), unless a value was set in the nanoseconds property, in which case the result date will have that number of nanoseconds (or as close as possible with floating point numbers).
    ///
    /// The enumeration is stopped by setting `stop` to `true` in the closure and returning. It is not necessary to set `stop` to `false` to keep the enumeration going.
    /// - parameter start: The `Date` at which to start the search.
    /// - parameter components: The `DateComponents` to use as input to the search algorithm.
    /// - parameter matchingPolicy: Determines the behavior of the search algorithm when the input produces an ambiguous result.
    /// - parameter repeatedTimePolicy: Determines the behavior of the search algorithm when the input produces a time that occurs twice on a particular day.
    /// - parameter direction: Which direction in time to search. The default value is `.forward`, which means later in time.
    /// - parameter block: A closure that is called with search results.
    @available(iOS 8.0, *)
    public func enumerateDates(startingAfter start: Date, matching components: DateComponents, matchingPolicy: MatchingPolicy, repeatedTimePolicy: RepeatedTimePolicy = .first, direction: SearchDirection = .forward, using block: (_ result: Date?, _ exactMatch: Bool, _ stop: inout Bool) -> Void) {
        _handle.map {
            $0.enumerateDates(startingAfter: start, matching: components, options: Calendar._toCalendarOptions(matchingPolicy: matchingPolicy, repeatedTimePolicy: repeatedTimePolicy, direction: direction)) { (result, exactMatch, stop) in
                var stopv = false
                block(result, exactMatch, &stopv)
                if stopv {
                    stop.pointee = true
                }
            }
        }
    }
    
    
    @available(*, unavailable, message: "use nextDate(after:matching:matchingPolicy:repeatedTimePolicy:direction:) instead")
    public func nextDate(after date: Date, matching comps: DateComponents, options: NSCalendar.Options = []) -> Date? { fatalError() }
    
    /// Computes the next date which matches (or most closely matches) a given set of components.
    ///
    /// The general semantics follow those of the `enumerateDates` function.
    /// To compute a sequence of results, use the `enumerateDates` function, rather than looping and calling this method with the previous loop iteration's result.
    /// - parameter date: The starting date.
    /// - parameter components: The components to search for.
    /// - parameter matchingPolicy: Specifies the technique the search algorithm uses to find results. Default value is `.nextTime`.
    /// - parameter repeatedTimePolicy: Specifies the behavior when multiple matches are found. Default value is `.first`.
    /// - parameter direction: Specifies the direction in time to search. Default is `.forward`.
    /// - returns: A `Date` representing the result of the search, or `nil` if a result could not be found.
    @available(iOS 8.0, *)
    public func nextDate(after date: Date, matching components: DateComponents, matchingPolicy: MatchingPolicy, repeatedTimePolicy: RepeatedTimePolicy = .first, direction: SearchDirection = .forward) -> Date? {
        return _handle.map { $0.nextDate(after: date, matching: components, options: Calendar._toCalendarOptions(matchingPolicy: matchingPolicy, repeatedTimePolicy: repeatedTimePolicy, direction: direction)) }
    }
    
    @available(*, unavailable, message: "use nextDate(after:matching:matchingPolicy:repeatedTimePolicy:direction:) instead")
    public func nextDate(after date: Date, matchingHour hourValue: Int, minute minuteValue: Int, second secondValue: Int, options: NSCalendar.Options = []) -> Date? { fatalError() }

    // MARK: -
    //
    
    @available(*, unavailable, renamed: "date(bySetting:value:of:)")
    public func date(bySettingUnit unit: NSCalendar.Unit, value v: Int, of date: Date, options opts: NSCalendar.Options = []) -> Date? { fatalError() }
    
    /// Returns a new `Date` representing the date calculated by setting a specific component to a given time, and trying to keep lower components the same.  If the component already has that value, this may result in a date which is the same as the given date.
    ///
    /// Changing a component's value often will require higher or coupled components to change as well.  For example, setting the Weekday to Thursday usually will require the Day component to change its value, and possibly the Month and Year as well.
    /// If no such time exists, the next available time is returned (which could, for example, be in a different day, week, month, ... than the nominal target date).  Setting a component to something which would be inconsistent forces other components to change; for example, setting the Weekday to Thursday probably shifts the Day and possibly Month and Year.
    /// The exact behavior of this method is implementation-defined. For example, if changing the weekday to Thursday, does that move forward to the next, backward to the previous, or to the nearest Thursday? The algorithm will try to produce a result which is in the next-larger component to the one given (there's a table of this mapping at the top of this document).  So for the "set to Thursday" example, find the Thursday in the Week in which the given date resides (which could be a forwards or backwards move, and not necessarily the nearest Thursday). For more control over the exact behavior, use `nextDate(after:matching:matchingPolicy:behavior:direction:)`.
    @available(iOS 8.0, *)
    public func date(bySetting component: Component, value: Int, of date: Date) -> Date? {
        return _handle.map { $0.date(bySettingUnit: Calendar._toCalendarUnit([component]), value: value, of: date, options: []) }
    }
    
    
    @available(*, unavailable, message: "use date(bySettingHour:minute:second:of:matchingPolicy:repeatedTimePolicy:direction:) instead")
    public func date(bySettingHour h: Int, minute m: Int, second s: Int, of date: Date, options opts: NSCalendar.Options = []) -> Date? { fatalError() }
    
    /// Returns a new `Date` representing the date calculated by setting hour, minute, and second to a given time on a specified `Date`.
    ///
    /// If no such time exists, the next available time is returned (which could, for example, be in a different day than the nominal target date).
    /// The intent is to return a date on the same day as the original date argument.  This may result in a date which is backward than the given date, of course.
    /// - parameter hour: A specified hour.
    /// - parameter minute: A specified minute.
    /// - parameter second: A specified second.
    /// - parameter date: The date to start calculation with.
    /// - parameter matchingPolicy: Specifies the technique the search algorithm uses to find results. Default value is `.nextTime`.
    /// - parameter repeatedTimePolicy: Specifies the behavior when multiple matches are found. Default value is `.first`.
    /// - parameter direction: Specifies the direction in time to search. Default is `.forward`.
    /// - returns: A `Date` representing the result of the search, or `nil` if a result could not be found.
    @available(iOS 8.0, *)
    public func date(bySettingHour hour: Int, minute: Int, second: Int, of date: Date, matchingPolicy: MatchingPolicy = .nextTime, repeatedTimePolicy: RepeatedTimePolicy = .first, direction: SearchDirection = .forward) -> Date? {
        return _handle.map { $0.date(bySettingHour: hour, minute: minute, second: second, of: date, options: Calendar._toCalendarOptions(matchingPolicy: matchingPolicy, repeatedTimePolicy: repeatedTimePolicy, direction: direction)) }
    }
    
    /// Determine if the `Date` has all of the specified `DateComponents`.
    ///
    /// It may be useful to test the return value of `nextDate(after:matching:matchingPolicy:behavior:direction:)` to find out if the components were obeyed or if the method had to fudge the result value due to missing time (for example, a daylight saving time transition).
    ///
    /// - returns: `true` if the date matches all of the components, otherwise `false`.
    @available(iOS 8.0, *)
    public func date(_ date: Date, matchesComponents components: DateComponents) -> Bool {
        return _handle.map { $0.date(date, matchesComponents: components) }
    }
    
    // MARK: -
    
    public var hashValue : Int {
        // We implement hash ourselves, because we need to make sure autoupdating calendars have the same hash
        if _autoupdating {
            return 1
        } else {
            return _handle.map { $0.hash }
        }
    }
    
    // MARK: -
    // MARK: Conversion Functions
    
    /// Turn our more-specific options into the big bucket option set of NSCalendar
    private static func _toCalendarOptions(matchingPolicy: MatchingPolicy, repeatedTimePolicy: RepeatedTimePolicy, direction: SearchDirection) -> NSCalendar.Options {
        var result : NSCalendar.Options = []
        
        switch matchingPolicy {
        case .nextTime:
            result.insert(.matchNextTime)
        case .nextTimePreservingSmallerComponents:
            result.insert(.matchNextTimePreservingSmallerUnits)
        case .previousTimePreservingSmallerComponents:
            result.insert(.matchPreviousTimePreservingSmallerUnits)
        case .strict:
            result.insert(.matchStrictly)
        }
        
        switch repeatedTimePolicy {
        case .first:
            result.insert(.matchFirst)
        case .last:
            result.insert(.matchLast)
        }
        
        switch direction {
        case .backward:
            result.insert(.searchBackwards)
        case .forward:
            break
        }
        
        return result
    }
    
    internal static func _toCalendarUnit(_ units : Set<Component>) -> NSCalendar.Unit {
        let unitMap : [Component : NSCalendar.Unit] =
            [.era : .era,
             .year : .year,
             .month : .month,
             .day : .day,
             .hour : .hour,
             .minute : .minute,
             .second : .second,
             .weekday : .weekday,
             .weekdayOrdinal : .weekdayOrdinal,
             .quarter : .quarter,
             .weekOfMonth : .weekOfMonth,
             .weekOfYear : .weekOfYear,
             .yearForWeekOfYear : .yearForWeekOfYear,
             .nanosecond : .nanosecond,
             .calendar : .calendar,
             .timeZone : .timeZone]
        
        var result = NSCalendar.Unit()
        for u in units {
            result.insert(unitMap[u]!)
        }
        return result
    }
    
    internal static func _toNSCalendarIdentifier(_ identifier : Identifier) -> NSCalendar.Identifier {
        if #available(macOS 10.10, iOS 8.0, *) {
            let identifierMap : [Identifier : NSCalendar.Identifier] =
                [.gregorian : .gregorian,
                 .buddhist : .buddhist,
                 .chinese : .chinese,
                 .coptic : .coptic,
                 .ethiopicAmeteMihret : .ethiopicAmeteMihret,
                 .ethiopicAmeteAlem : .ethiopicAmeteAlem,
                 .hebrew : .hebrew,
                 .iso8601 : .ISO8601,
                 .indian : .indian,
                 .islamic : .islamic,
                 .islamicCivil : .islamicCivil,
                 .japanese : .japanese,
                 .persian : .persian,
                 .republicOfChina : .republicOfChina,
                 .islamicTabular : .islamicTabular,
                 .islamicUmmAlQura : .islamicUmmAlQura]
            return identifierMap[identifier]!
        } else {
            let identifierMap : [Identifier : NSCalendar.Identifier] =
                [.gregorian : .gregorian,
                 .buddhist : .buddhist,
                 .chinese : .chinese,
                 .coptic : .coptic,
                 .ethiopicAmeteMihret : .ethiopicAmeteMihret,
                 .ethiopicAmeteAlem : .ethiopicAmeteAlem,
                 .hebrew : .hebrew,
                 .iso8601 : .ISO8601,
                 .indian : .indian,
                 .islamic : .islamic,
                 .islamicCivil : .islamicCivil,
                 .japanese : .japanese,
                 .persian : .persian,
                 .republicOfChina : .republicOfChina]
            return identifierMap[identifier]!
        }
    }
    
    internal static func _fromNSCalendarIdentifier(_ identifier : NSCalendar.Identifier) -> Identifier {
        if #available(macOS 10.10, iOS 8.0, *) {
            let identifierMap : [NSCalendar.Identifier : Identifier] =
                [.gregorian : .gregorian,
                 .buddhist : .buddhist,
                 .chinese : .chinese,
                 .coptic : .coptic,
                 .ethiopicAmeteMihret : .ethiopicAmeteMihret,
                 .ethiopicAmeteAlem : .ethiopicAmeteAlem,
                 .hebrew : .hebrew,
                 .ISO8601 : .iso8601,
                 .indian : .indian,
                 .islamic : .islamic,
                 .islamicCivil : .islamicCivil,
                 .japanese : .japanese,
                 .persian : .persian,
                 .republicOfChina : .republicOfChina,
                 .islamicTabular : .islamicTabular,
                 .islamicUmmAlQura : .islamicUmmAlQura]
            return identifierMap[identifier]!
        } else {
            let identifierMap : [NSCalendar.Identifier : Identifier] =
                [.gregorian : .gregorian,
                 .buddhist : .buddhist,
                 .chinese : .chinese,
                 .coptic : .coptic,
                 .ethiopicAmeteMihret : .ethiopicAmeteMihret,
                 .ethiopicAmeteAlem : .ethiopicAmeteAlem,
                 .hebrew : .hebrew,
                 .ISO8601 : .iso8601,
                 .indian : .indian,
                 .islamic : .islamic,
                 .islamicCivil : .islamicCivil,
                 .japanese : .japanese,
                 .persian : .persian,
                 .republicOfChina : .republicOfChina]
            return identifierMap[identifier]!
        }
    }

    public static func ==(lhs: Calendar, rhs: Calendar) -> Bool {
        if lhs._autoupdating || rhs._autoupdating {
            return lhs._autoupdating == rhs._autoupdating
        } else {
            // NSCalendar's isEqual is broken (27019864) so we must implement this ourselves
            return lhs.identifier == rhs.identifier &&
                lhs.locale == rhs.locale &&
                lhs.timeZone == rhs.timeZone &&
                lhs.firstWeekday == rhs.firstWeekday &&
                lhs.minimumDaysInFirstWeek == rhs.minimumDaysInFirstWeek
        }
    }

}

extension Calendar : CustomDebugStringConvertible, CustomStringConvertible, CustomReflectable {
    private var _kindDescription : String {
        if self == Calendar.autoupdatingCurrent {
            return "autoupdatingCurrent"
        } else if self == Calendar.current {
            return "current"
        } else {
            return "fixed"
        }
    }
    
    public var description: String {
        return "\(identifier) (\(_kindDescription))"
    }
    
    public var debugDescription : String {
        return "\(identifier) (\(_kindDescription))"
    }
    
    public var customMirror : Mirror {
        let c: [(label: String?, value: Any)] = [
          ("identifier", identifier),
          ("kind", _kindDescription),
          ("locale", locale as Any),
          ("timeZone", timeZone),
          ("firstWeekday", firstWeekday),
          ("minimumDaysInFirstWeek", minimumDaysInFirstWeek),
        ]
        return Mirror(self, children: c, displayStyle: Mirror.DisplayStyle.struct)
    }
}

extension Calendar : _ObjectiveCBridgeable {
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSCalendar {
        return _handle._copiedReference()
    }
    
    public static func _forceBridgeFromObjectiveC(_ input: NSCalendar, result: inout Calendar?) {
        if !_conditionallyBridgeFromObjectiveC(input, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ input: NSCalendar, result: inout Calendar?) -> Bool {
        result = Calendar(reference: input)
        return true
    }
    
    @_effects(readonly)
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSCalendar?) -> Calendar {
        var result: Calendar?
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension NSCalendar : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as Calendar)
    }
}

extension Calendar : Codable {
    private enum CodingKeys : Int, CodingKey {
        case identifier
        case locale
        case timeZone
        case firstWeekday
        case minimumDaysInFirstWeek
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let identifierString = try container.decode(String.self, forKey: .identifier)
        let identifier = Calendar._fromNSCalendarIdentifier(NSCalendar.Identifier(rawValue: identifierString))
        self.init(identifier: identifier)

        self.locale = try container.decodeIfPresent(Locale.self, forKey: .locale)
        self.timeZone = try container.decode(TimeZone.self, forKey: .timeZone)
        self.firstWeekday = try container.decode(Int.self, forKey: .firstWeekday)
        self.minimumDaysInFirstWeek = try container.decode(Int.self, forKey: .minimumDaysInFirstWeek)
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)

        let identifier = Calendar._toNSCalendarIdentifier(self.identifier).rawValue
        try container.encode(identifier, forKey: .identifier)
        try container.encode(self.locale, forKey: .locale)
        try container.encode(self.timeZone, forKey: .timeZone)
        try container.encode(self.firstWeekday, forKey: .firstWeekday)
        try container.encode(self.minimumDaysInFirstWeek, forKey: .minimumDaysInFirstWeek)
    }
}
