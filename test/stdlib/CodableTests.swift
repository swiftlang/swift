// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import CoreGraphics

#if FOUNDATION_XCTEST
import XCTest
class TestCodableSuper : XCTestCase { }
#else
import StdlibUnittest
class TestCodableSuper { }
#endif

// MARK: - Helper Functions
@available(OSX 10.11, iOS 9.0, *)
func makePersonNameComponents(namePrefix: String? = nil,
                              givenName: String? = nil,
                              middleName: String? = nil,
                              familyName: String? = nil,
                              nameSuffix: String? = nil,
                              nickname: String? = nil) -> PersonNameComponents {
    var result = PersonNameComponents()
    result.namePrefix = namePrefix
    result.givenName = givenName
    result.middleName = middleName
    result.familyName = familyName
    result.nameSuffix = nameSuffix
    result.nickname = nickname
    return result
}

func expectRoundTripEquality<T : Codable>(of value: T, encode: (T) throws -> Data, decode: (Data) throws -> T) where T : Equatable {
    let data: Data
    do {
        data = try encode(value)
    } catch {
        fatalError("Unable to encode \(T.self) <\(value)>: \(error)")
    }

    let decoded: T
    do {
        decoded = try decode(data)
    } catch {
        fatalError("Unable to decode \(T.self) <\(value)>: \(error)")
    }

    expectEqual(value, decoded, "Decoded \(T.self) <\(decoded)> not equal to original <\(value)>")
}

func expectRoundTripEqualityThroughJSON<T : Codable>(for value: T) where T : Equatable {
    let inf = "INF", negInf = "-INF", nan = "NaN"
    let encode = { (_ value: T) throws -> Data in
        let encoder = JSONEncoder()
        encoder.nonConformingFloatEncodingStrategy = .convertToString(positiveInfinity: inf,
                                                                      negativeInfinity: negInf,
                                                                      nan: nan)
        return try encoder.encode(value)
    }

    let decode = { (_ data: Data) throws -> T in
        let decoder = JSONDecoder()
        decoder.nonConformingFloatDecodingStrategy = .convertFromString(positiveInfinity: inf,
                                                                        negativeInfinity: negInf,
                                                                        nan: nan)
        return try decoder.decode(T.self, from: data)
    }

    expectRoundTripEquality(of: value, encode: encode, decode: decode)
}

func expectRoundTripEqualityThroughPlist<T : Codable>(for value: T) where T : Equatable {
    let encode = { (_ value: T) throws -> Data in
        return try PropertyListEncoder().encode(value)
    }

    let decode = { (_ data: Data) throws -> T in
        return try PropertyListDecoder().decode(T.self, from: data)
    }

    expectRoundTripEquality(of: value, encode: encode, decode: decode)
}

// MARK: - Helper Types
// A wrapper around a UUID that will allow it to be encoded at the top level of an encoder.
struct UUIDCodingWrapper : Codable, Equatable {
    let value: UUID

    init(_ value: UUID) {
        self.value = value
    }

    static func ==(_ lhs: UUIDCodingWrapper, _ rhs: UUIDCodingWrapper) -> Bool {
        return lhs.value == rhs.value
    }
}

// MARK: - Tests
class TestCodable : TestCodableSuper {
    // MARK: - AffineTransform
#if os(OSX)
    // FIXME: Comment the tests back in once rdar://problem/33363218 is in the SDK.
    lazy var affineTransformValues: [AffineTransform] = [
        AffineTransform.identity,
        AffineTransform(),
        AffineTransform(translationByX: 2.0, byY: 2.0),
        AffineTransform(scale: 2.0),
        AffineTransform(rotationByDegrees: .pi / 2),

        AffineTransform(m11: 1.0, m12: 2.5, m21: 66.2, m22: 40.2, tX: -5.5, tY: 3.7),
        // AffineTransform(m11: -55.66, m12: 22.7, m21: 1.5, m22: 0.0, tX: -22, tY: -33),
        AffineTransform(m11: 4.5, m12: 1.1, m21: 0.025, m22: 0.077, tX: -0.55, tY: 33.2),
        // AffineTransform(m11: 7.0, m12: -2.3, m21: 6.7, m22: 0.25, tX: 0.556, tY: 0.99),
        // AffineTransform(m11: 0.498, m12: -0.284, m21: -0.742, m22: 0.3248, tX: 12, tY: 44)
    ]

    func test_AffineTransform_JSON() {
        for transform in affineTransformValues {
            expectRoundTripEqualityThroughJSON(for: transform)
        }
    }

    func test_AffineTransform_Plist() {
        for transform in affineTransformValues {
            expectRoundTripEqualityThroughPlist(for: transform)
        }
    }
#endif

    // MARK: - Calendar
    lazy var calendarValues: [Calendar] = [
        Calendar(identifier: .gregorian),
        Calendar(identifier: .buddhist),
        Calendar(identifier: .chinese),
        Calendar(identifier: .coptic),
        Calendar(identifier: .ethiopicAmeteMihret),
        Calendar(identifier: .ethiopicAmeteAlem),
        Calendar(identifier: .hebrew),
        Calendar(identifier: .iso8601),
        Calendar(identifier: .indian),
        Calendar(identifier: .islamic),
        Calendar(identifier: .islamicCivil),
        Calendar(identifier: .japanese),
        Calendar(identifier: .persian),
        Calendar(identifier: .republicOfChina),
    ]

    func test_Calendar_JSON() {
        for calendar in calendarValues {
            expectRoundTripEqualityThroughJSON(for: calendar)
        }
    }

    func test_Calendar_Plist() {
        for calendar in calendarValues {
            expectRoundTripEqualityThroughPlist(for: calendar)
        }
    }

    // MARK: - CharacterSet
    lazy var characterSetValues: [CharacterSet] = [
        CharacterSet.controlCharacters,
        CharacterSet.whitespaces,
        CharacterSet.whitespacesAndNewlines,
        CharacterSet.decimalDigits,
        CharacterSet.letters,
        CharacterSet.lowercaseLetters,
        CharacterSet.uppercaseLetters,
        CharacterSet.nonBaseCharacters,
        CharacterSet.alphanumerics,
        CharacterSet.decomposables,
        CharacterSet.illegalCharacters,
        CharacterSet.punctuationCharacters,
        CharacterSet.capitalizedLetters,
        CharacterSet.symbols,
        CharacterSet.newlines
    ]

    func test_CharacterSet_JSON() {
        for characterSet in characterSetValues {
            expectRoundTripEqualityThroughJSON(for: characterSet)
        }
    }

    func test_CharacterSet_Plist() {
        for characterSet in characterSetValues {
            expectRoundTripEqualityThroughPlist(for: characterSet)
        }
    }

    // MARK: - CGAffineTransform
    lazy var cg_affineTransformValues: [CGAffineTransform] = {
        // FIXME: Comment the tests back in once rdar://problem/33363218 is in the SDK.
        var values = [
            CGAffineTransform.identity,
            CGAffineTransform(),
            CGAffineTransform(translationX: 2.0, y: 2.0),
            CGAffineTransform(scaleX: 2.0, y: 2.0),
            CGAffineTransform(a: 1.0, b: 2.5, c: 66.2, d: 40.2, tx: -5.5, ty: 3.7),
            // CGAffineTransform(a: -55.66, b: 22.7, c: 1.5, d: 0.0, tx: -22, ty: -33),
            CGAffineTransform(a: 4.5, b: 1.1, c: 0.025, d: 0.077, tx: -0.55, ty: 33.2),
            // CGAffineTransform(a: 7.0, b: -2.3, c: 6.7, d: 0.25, tx: 0.556, ty: 0.99),
            // CGAffineTransform(a: 0.498, b: -0.284, c: -0.742, d: 0.3248, tx: 12, ty: 44)
        ]

        if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
            values.append(CGAffineTransform(rotationAngle: .pi / 2))
        }

        return values
    }()

    func test_CGAffineTransform_JSON() {
        for transform in cg_affineTransformValues {
            expectRoundTripEqualityThroughJSON(for: transform)
        }
    }

    func test_CGAffineTransform_Plist() {
        for transform in cg_affineTransformValues {
            expectRoundTripEqualityThroughPlist(for: transform)
        }
    }

    // MARK: - CGPoint
    lazy var cg_pointValues: [CGPoint] = {
        var values = [
            CGPoint.zero,
            CGPoint(x: 10, y: 20)
        ]

        if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
            // Limit on magnitude in JSON. See rdar://problem/12717407
            values.append(CGPoint(x: CGFloat.greatestFiniteMagnitude,
                                  y: CGFloat.greatestFiniteMagnitude))
        }

        return values
    }()

    func test_CGPoint_JSON() {
        for point in cg_pointValues {
            expectRoundTripEqualityThroughJSON(for: point)
        }
    }

    func test_CGPoint_Plist() {
        for point in cg_pointValues {
            expectRoundTripEqualityThroughPlist(for: point)
        }
    }

    // MARK: - CGSize
    lazy var cg_sizeValues: [CGSize] = {
        var values = [
            CGSize.zero,
            CGSize(width: 30, height: 40)
        ]

        if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
            // Limit on magnitude in JSON. See rdar://problem/12717407
            values.append(CGSize(width: CGFloat.greatestFiniteMagnitude,
                                 height: CGFloat.greatestFiniteMagnitude))
        }

        return values
    }()

    func test_CGSize_JSON() {
        for size in cg_sizeValues {
            expectRoundTripEqualityThroughJSON(for: size)
        }
    }

    func test_CGSize_Plist() {
        for size in cg_sizeValues {
            expectRoundTripEqualityThroughPlist(for: size)
        }
    }

    // MARK: - CGRect
    lazy var cg_rectValues: [CGRect] = {
        // FIXME: Comment the tests back in once rdar://problem/33363218 is in the SDK.
        var values = [
            CGRect.zero,
            // CGRect.null,
            CGRect(x: 10, y: 20, width: 30, height: 40)
        ]

        if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
            // Limit on magnitude in JSON. See rdar://problem/12717407
            // values.append(CGRect.infinite)
        }

        return values
    }()

    func test_CGRect_JSON() {
        for rect in cg_rectValues {
            expectRoundTripEqualityThroughJSON(for: rect)
        }
    }

    func test_CGRect_Plist() {
        for rect in cg_rectValues {
            expectRoundTripEqualityThroughPlist(for: rect)
        }
    }

    // MARK: - CGVector
    lazy var cg_vectorValues: [CGVector] = {
        // FIXME: Comment the tests back in once rdar://problem/33363218 is in the SDK.
        var values = [
            CGVector.zero,
            // CGVector(dx: 0.0, dy: -9.81)
        ]

        if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
            // Limit on magnitude in JSON. See rdar://problem/12717407
            values.append(CGVector(dx: CGFloat.greatestFiniteMagnitude,
                                   dy: CGFloat.greatestFiniteMagnitude))
        }

        return values
    }()

    func test_CGVector_JSON() {
        for vector in cg_vectorValues {
            expectRoundTripEqualityThroughJSON(for: vector)
        }
    }

    func test_CGVector_Plist() {
        for vector in cg_vectorValues {
            expectRoundTripEqualityThroughPlist(for: vector)
        }
    }

    // MARK: - DateComponents
    lazy var dateComponents: Set<Calendar.Component> = [
        .era, .year, .month, .day, .hour, .minute, .second, .nanosecond,
        .weekday, .weekdayOrdinal, .quarter, .weekOfMonth, .weekOfYear,
        .yearForWeekOfYear, .timeZone, .calendar
    ]

    func test_DateComponents_JSON() {
        let calendar = Calendar(identifier: .gregorian)
        let components = calendar.dateComponents(dateComponents, from: Date())
        expectRoundTripEqualityThroughJSON(for: components)
    }

    func test_DateComponents_Plist() {
        let calendar = Calendar(identifier: .gregorian)
        let components = calendar.dateComponents(dateComponents, from: Date())
        expectRoundTripEqualityThroughPlist(for: components)
    }

    // MARK: - DateInterval
    @available(OSX 10.12, iOS 10.10, watchOS 3.0, tvOS 10.0, *)
    lazy var dateIntervalValues: [DateInterval] = [
        DateInterval(),
        DateInterval(start: Date.distantPast, end: Date()),
        DateInterval(start: Date(), end: Date.distantFuture),
        DateInterval(start: Date.distantPast, end: Date.distantFuture)
    ]

    @available(OSX 10.12, iOS 10.10, watchOS 3.0, tvOS 10.0, *)
    func test_DateInterval_JSON() {
        for interval in dateIntervalValues {
            expectRoundTripEqualityThroughJSON(for: interval)
        }
    }

    @available(OSX 10.12, iOS 10.10, watchOS 3.0, tvOS 10.0, *)
    func test_DateInterval_Plist() {
        for interval in dateIntervalValues {
            expectRoundTripEqualityThroughPlist(for: interval)
        }
    }

    // MARK: - Decimal
    lazy var decimalValues: [Decimal] = [
        Decimal.leastFiniteMagnitude,
        Decimal.greatestFiniteMagnitude,
        Decimal.leastNormalMagnitude,
        Decimal.leastNonzeroMagnitude,
        Decimal(),

        // Decimal.pi does not round-trip at the moment.
        // See rdar://problem/33165355
        // Decimal.pi,
    ]

    func test_Decimal_JSON() {
        for decimal in decimalValues {
            // Decimal encodes as a number in JSON and cannot be encoded at the top level.
            expectRoundTripEqualityThroughJSON(for: TopLevelWrapper(decimal))
        }
    }

    func test_Decimal_Plist() {
        for decimal in decimalValues {
            expectRoundTripEqualityThroughPlist(for: decimal)
        }
    }

    // MARK: - IndexPath
    lazy var indexPathValues: [IndexPath] = [
        IndexPath(), // empty
        IndexPath(index: 0), // single
        IndexPath(indexes: [1, 2]), // pair
        IndexPath(indexes: [3, 4, 5, 6, 7, 8]), // array
    ]

    func test_IndexPath_JSON() {
        for indexPath in indexPathValues {
            expectRoundTripEqualityThroughJSON(for: indexPath)
        }
    }

    func test_IndexPath_Plist() {
        for indexPath in indexPathValues {
            expectRoundTripEqualityThroughPlist(for: indexPath)
        }
    }

    // MARK: - IndexSet
    lazy var indexSetValues: [IndexSet] = [
        IndexSet(),
        IndexSet(integer: 42),
        IndexSet(integersIn: 0 ..< Int.max)
    ]

    func test_IndexSet_JSON() {
        for indexSet in indexSetValues {
            expectRoundTripEqualityThroughJSON(for: indexSet)
        }
    }

    func test_IndexSet_Plist() {
        for indexSet in indexSetValues {
            expectRoundTripEqualityThroughPlist(for: indexSet)
        }
    }

    // MARK: - Locale
    lazy var localeValues: [Locale] = [
        Locale(identifier: ""),
        Locale(identifier: "en"),
        Locale(identifier: "en_US"),
        Locale(identifier: "en_US_POSIX"),
        Locale(identifier: "uk"),
        Locale(identifier: "fr_FR"),
        Locale(identifier: "fr_BE"),
        Locale(identifier: "zh-Hant-HK")
    ]

    func test_Locale_JSON() {
        for locale in localeValues {
            expectRoundTripEqualityThroughJSON(for: locale)
        }
    }

    func test_Locale_Plist() {
        for locale in localeValues {
            expectRoundTripEqualityThroughPlist(for: locale)
        }
    }

    // MARK: - Measurement
    @available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
    lazy var unitValues: [Dimension] = [
        UnitAcceleration.metersPerSecondSquared,
        UnitMass.kilograms,
        UnitLength.miles
    ]

    @available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
    func test_Measurement_JSON() {
        for unit in unitValues {
            expectRoundTripEqualityThroughJSON(for: Measurement(value: 42, unit: unit))
        }
    }

    @available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
    func test_Measurement_Plist() {
        for unit in unitValues {
            expectRoundTripEqualityThroughJSON(for: Measurement(value: 42, unit: unit))
        }
    }

    // MARK: - NSRange
    lazy var nsrangeValues: [NSRange] = [
        NSRange(),
        NSRange(location: 0, length: Int.max),
        NSRange(location: NSNotFound, length: 0),
    ]

    func test_NSRange_JSON() {
        for range in nsrangeValues {
            expectRoundTripEqualityThroughJSON(for: range)
        }
    }

    func test_NSRange_Plist() {
        for range in nsrangeValues {
            expectRoundTripEqualityThroughPlist(for: range)
        }
    }

    // MARK: - PersonNameComponents
    @available(OSX 10.11, iOS 9.0, *)
    lazy var personNameComponentsValues: [PersonNameComponents] = [
        makePersonNameComponents(givenName: "John", familyName: "Appleseed"),
        makePersonNameComponents(givenName: "John", familyName: "Appleseed", nickname: "Johnny"),
        makePersonNameComponents(namePrefix: "Dr.", givenName: "Jane", middleName: "A.", familyName: "Appleseed", nameSuffix: "Esq.", nickname: "Janie")
    ]

    @available(OSX 10.11, iOS 9.0, *)
    func test_PersonNameComponents_JSON() {
        for components in personNameComponentsValues {
            expectRoundTripEqualityThroughJSON(for: components)
        }
    }

    @available(OSX 10.11, iOS 9.0, *)
    func test_PersonNameComponents_Plist() {
        for components in personNameComponentsValues {
            expectRoundTripEqualityThroughPlist(for: components)
        }
    }

    // MARK: - TimeZone
    lazy var timeZoneValues: [TimeZone] = [
        TimeZone(identifier: "America/Los_Angeles")!,
        TimeZone(identifier: "UTC")!,
        TimeZone.current
    ]

    func test_TimeZone_JSON() {
        for timeZone in timeZoneValues {
            expectRoundTripEqualityThroughJSON(for: timeZone)
        }
    }

    func test_TimeZone_Plist() {
        for timeZone in timeZoneValues {
            expectRoundTripEqualityThroughPlist(for: timeZone)
        }
    }

    // MARK: - URL
    lazy var urlValues: [URL] = {
        var values: [URL] = [
            URL(fileURLWithPath: NSTemporaryDirectory()),
            URL(fileURLWithPath: "/"),
            URL(string: "http://apple.com")!,
            URL(string: "swift", relativeTo: URL(string: "http://apple.com")!)!
        ]

        if #available(OSX 10.11, iOS 9.0, *) {
            values.append(URL(fileURLWithPath: "bin/sh", relativeTo: URL(fileURLWithPath: "/")))
        }

        return values
    }()

    func test_URL_JSON() {
        for url in urlValues {
            // URLs encode as single strings in JSON. They lose their baseURL this way.
            // For relative URLs, we don't expect them to be equal to the original.
            if url.baseURL == nil {
                // This is an absolute URL; we can expect equality.
                expectRoundTripEqualityThroughJSON(for: TopLevelWrapper(url))
            } else {
                // This is a relative URL. Make it absolute first.
                let absoluteURL = URL(string: url.absoluteString)!
                expectRoundTripEqualityThroughJSON(for: TopLevelWrapper(absoluteURL))
            }
        }
    }

    func test_URL_Plist() {
        for url in urlValues {
            expectRoundTripEqualityThroughPlist(for: url)
        }
    }

    // MARK: - UUID
    lazy var uuidValues: [UUID] = [
        UUID(),
        UUID(uuidString: "E621E1F8-C36C-495A-93FC-0C247A3E6E5F")!,
        UUID(uuidString: "e621e1f8-c36c-495a-93fc-0c247a3e6e5f")!,
        UUID(uuid: uuid_t(0xe6,0x21,0xe1,0xf8,0xc3,0x6c,0x49,0x5a,0x93,0xfc,0x0c,0x24,0x7a,0x3e,0x6e,0x5f))
    ]

    func test_UUID_JSON() {
        for uuid in uuidValues {
            // We have to wrap the UUID since we cannot have a top-level string.
            expectRoundTripEqualityThroughJSON(for: UUIDCodingWrapper(uuid))
        }
    }

    func test_UUID_Plist() {
        for uuid in uuidValues {
            // We have to wrap the UUID since we cannot have a top-level string.
            expectRoundTripEqualityThroughPlist(for: UUIDCodingWrapper(uuid))
        }
    }
}

// MARK: - Helper Types

struct TopLevelWrapper<T> : Codable, Equatable where T : Codable, T : Equatable {
    let value: T

    init(_ value: T) {
        self.value = value
    }

    static func ==(_ lhs: TopLevelWrapper<T>, _ rhs: TopLevelWrapper<T>) -> Bool {
        return lhs.value == rhs.value
    }
}

// MARK: - Tests

#if !FOUNDATION_XCTEST
var CodableTests = TestSuite("TestCodable")

#if os(OSX)
    CodableTests.test("test_AffineTransform_JSON") { TestCodable().test_AffineTransform_JSON() }
    CodableTests.test("test_AffineTransform_Plist") { TestCodable().test_AffineTransform_Plist() }
#endif

CodableTests.test("test_Calendar_JSON") { TestCodable().test_Calendar_JSON() }
CodableTests.test("test_Calendar_Plist") { TestCodable().test_Calendar_Plist() }
CodableTests.test("test_CharacterSet_JSON") { TestCodable().test_CharacterSet_JSON() }
CodableTests.test("test_CharacterSet_Plist") { TestCodable().test_CharacterSet_Plist() }
CodableTests.test("test_CGAffineTransform_JSON") { TestCodable().test_CGAffineTransform_JSON() }
CodableTests.test("test_CGAffineTransform_Plist") { TestCodable().test_CGAffineTransform_Plist() }
CodableTests.test("test_CGPoint_JSON") { TestCodable().test_CGPoint_JSON() }
CodableTests.test("test_CGPoint_Plist") { TestCodable().test_CGPoint_Plist() }
CodableTests.test("test_CGSize_JSON") { TestCodable().test_CGSize_JSON() }
CodableTests.test("test_CGSize_Plist") { TestCodable().test_CGSize_Plist() }
CodableTests.test("test_CGRect_JSON") { TestCodable().test_CGRect_JSON() }
CodableTests.test("test_CGRect_Plist") { TestCodable().test_CGRect_Plist() }
CodableTests.test("test_CGVector_JSON") { TestCodable().test_CGVector_JSON() }
CodableTests.test("test_CGVector_Plist") { TestCodable().test_CGVector_Plist() }
CodableTests.test("test_DateComponents_JSON") { TestCodable().test_DateComponents_JSON() }
CodableTests.test("test_DateComponents_Plist") { TestCodable().test_DateComponents_Plist() }

if #available(OSX 10.12, iOS 10.10, watchOS 3.0, tvOS 10.0, *) {
    // CodableTests.test("test_DateInterval_JSON") { TestCodable().test_DateInterval_JSON() }
    CodableTests.test("test_DateInterval_Plist") { TestCodable().test_DateInterval_Plist() }
}

CodableTests.test("test_Decimal_JSON") { TestCodable().test_Decimal_JSON() }
CodableTests.test("test_Decimal_Plist") { TestCodable().test_Decimal_Plist() }
CodableTests.test("test_IndexPath_JSON") { TestCodable().test_IndexPath_JSON() }
CodableTests.test("test_IndexPath_Plist") { TestCodable().test_IndexPath_Plist() }
CodableTests.test("test_IndexSet_JSON") { TestCodable().test_IndexSet_JSON() }
CodableTests.test("test_IndexSet_Plist") { TestCodable().test_IndexSet_Plist() }
CodableTests.test("test_Locale_JSON") { TestCodable().test_Locale_JSON() }
CodableTests.test("test_Locale_Plist") { TestCodable().test_Locale_Plist() }

if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
    // CodableTests.test("test_Measurement_JSON") { TestCodable().test_Measurement_JSON() }
    // CodableTests.test("test_Measurement_Plist") { TestCodable().test_Measurement_Plist() }
}

CodableTests.test("test_NSRange_JSON") { TestCodable().test_NSRange_JSON() }
CodableTests.test("test_NSRange_Plist") { TestCodable().test_NSRange_Plist() }

if #available(OSX 10.11, iOS 9.0, *) {
    CodableTests.test("test_PersonNameComponents_JSON") { TestCodable().test_PersonNameComponents_JSON() }
    CodableTests.test("test_PersonNameComponents_Plist") { TestCodable().test_PersonNameComponents_Plist() }
}

CodableTests.test("test_TimeZone_JSON") { TestCodable().test_TimeZone_JSON() }
CodableTests.test("test_TimeZone_Plist") { TestCodable().test_TimeZone_Plist() }
CodableTests.test("test_URL_JSON") { TestCodable().test_URL_JSON() }
CodableTests.test("test_URL_Plist") { TestCodable().test_URL_Plist() }
CodableTests.test("test_UUID_JSON") { TestCodable().test_UUID_JSON() }
CodableTests.test("test_UUID_Plist") { TestCodable().test_UUID_Plist() }
runAllTests()
#endif
