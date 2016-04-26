
var kABMultiValueMask: Int32 { get }
struct _ABPropertyType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kABErrorInProperty: _ABPropertyType { get }
var kABStringProperty: _ABPropertyType { get }
var kABIntegerProperty: _ABPropertyType { get }
var kABRealProperty: _ABPropertyType { get }
var kABDateProperty: _ABPropertyType { get }
var kABArrayProperty: _ABPropertyType { get }
var kABDictionaryProperty: _ABPropertyType { get }
var kABDataProperty: _ABPropertyType { get }
var kABDateComponentsProperty: _ABPropertyType { get }
var kABMultiStringProperty: _ABPropertyType { get }
var kABMultiIntegerProperty: _ABPropertyType { get }
var kABMultiRealProperty: _ABPropertyType { get }
var kABMultiDateProperty: _ABPropertyType { get }
var kABMultiArrayProperty: _ABPropertyType { get }
var kABMultiDictionaryProperty: _ABPropertyType { get }
var kABMultiDataProperty: _ABPropertyType { get }
var kABMultiDateComponentsProperty: _ABPropertyType { get }
typealias ABPropertyType = CFIndex
struct _ABSearchComparison : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kABEqual: _ABSearchComparison { get }
var kABNotEqual: _ABSearchComparison { get }
var kABLessThan: _ABSearchComparison { get }
var kABLessThanOrEqual: _ABSearchComparison { get }
var kABGreaterThan: _ABSearchComparison { get }
var kABGreaterThanOrEqual: _ABSearchComparison { get }
var kABEqualCaseInsensitive: _ABSearchComparison { get }
var kABContainsSubString: _ABSearchComparison { get }
var kABContainsSubStringCaseInsensitive: _ABSearchComparison { get }
var kABPrefixMatch: _ABSearchComparison { get }
var kABPrefixMatchCaseInsensitive: _ABSearchComparison { get }
var kABBitsInBitFieldMatch: _ABSearchComparison { get }
var kABDoesNotContainSubString: _ABSearchComparison { get }
var kABDoesNotContainSubStringCaseInsensitive: _ABSearchComparison { get }
var kABNotEqualCaseInsensitive: _ABSearchComparison { get }
var kABSuffixMatch: _ABSearchComparison { get }
var kABSuffixMatchCaseInsensitive: _ABSearchComparison { get }
var kABWithinIntervalAroundToday: _ABSearchComparison { get }
var kABWithinIntervalAroundTodayYearless: _ABSearchComparison { get }
var kABNotWithinIntervalAroundToday: _ABSearchComparison { get }
var kABNotWithinIntervalAroundTodayYearless: _ABSearchComparison { get }
var kABWithinIntervalFromToday: _ABSearchComparison { get }
var kABWithinIntervalFromTodayYearless: _ABSearchComparison { get }
var kABNotWithinIntervalFromToday: _ABSearchComparison { get }
var kABNotWithinIntervalFromTodayYearless: _ABSearchComparison { get }
typealias ABSearchComparison = CFIndex
struct _ABSearchConjunction : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kABSearchAnd: _ABSearchConjunction { get }
var kABSearchOr: _ABSearchConjunction { get }
typealias ABSearchConjunction = CFIndex
