
struct CMTimeRange {
  var start: CMTime
  var duration: CMTime
  init()
  init(start start: CMTime, duration duration: CMTime)
}

extension CMTimeRange {
  init(start start: CMTime, end end: CMTime)
  var isValid: Bool { get }
  var isIndefinite: Bool { get }
  var isEmpty: Bool { get }
  var end: CMTime { get }
  @warn_unused_result
  func union(_ otherRange: CMTimeRange) -> CMTimeRange
  @warn_unused_result
  func intersection(_ otherRange: CMTimeRange) -> CMTimeRange
  @warn_unused_result
  func containsTime(_ time: CMTime) -> Bool
  @warn_unused_result
  func containsTimeRange(_ range: CMTimeRange) -> Bool
}

extension CMTimeRange : Equatable {
}
@available(tvOS 4.0, *)
let kCMTimeRangeZero: CMTimeRange
@available(tvOS 4.0, *)
let kCMTimeRangeInvalid: CMTimeRange
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeMake(_ start: CMTime, _ duration: CMTime) -> CMTimeRange
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeGetUnion(_ range1: CMTimeRange, _ range2: CMTimeRange) -> CMTimeRange
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeGetIntersection(_ range1: CMTimeRange, _ range2: CMTimeRange) -> CMTimeRange
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeEqual(_ range1: CMTimeRange, _ range2: CMTimeRange) -> Bool
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeContainsTime(_ range: CMTimeRange, _ time: CMTime) -> Bool
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeContainsTimeRange(_ range1: CMTimeRange, _ range2: CMTimeRange) -> Bool
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeGetEnd(_ range: CMTimeRange) -> CMTime
@available(tvOS 4.0, *)
@discardableResult
func CMTimeMapTimeFromRangeToRange(_ t: CMTime, _ fromRange: CMTimeRange, _ toRange: CMTimeRange) -> CMTime
@available(tvOS 4.0, *)
@discardableResult
func CMTimeClampToRange(_ time: CMTime, _ range: CMTimeRange) -> CMTime
@available(tvOS 4.0, *)
@discardableResult
func CMTimeMapDurationFromRangeToRange(_ dur: CMTime, _ fromRange: CMTimeRange, _ toRange: CMTimeRange) -> CMTime
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeFromTimeToTime(_ start: CMTime, _ end: CMTime) -> CMTimeRange
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeCopyAsDictionary(_ range: CMTimeRange, _ allocator: CFAllocator?) -> CFDictionary?
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeMakeFromDictionary(_ dict: CFDictionary) -> CMTimeRange
@available(tvOS 4.0, *)
let kCMTimeRangeStartKey: CFString
@available(tvOS 4.0, *)
let kCMTimeRangeDurationKey: CFString
@available(tvOS 4.0, *)
@discardableResult
func CMTimeRangeCopyDescription(_ allocator: CFAllocator?, _ range: CMTimeRange) -> CFString?
@available(tvOS 4.0, *)
func CMTimeRangeShow(_ range: CMTimeRange)
struct CMTimeMapping {
  var source: CMTimeRange
  var target: CMTimeRange
  init()
  init(source source: CMTimeRange, target target: CMTimeRange)
}
@available(tvOS 9.0, *)
let kCMTimeMappingInvalid: CMTimeMapping
@available(tvOS 9.0, *)
@discardableResult
func CMTimeMappingMake(_ source: CMTimeRange, _ target: CMTimeRange) -> CMTimeMapping
@available(tvOS 9.0, *)
@discardableResult
func CMTimeMappingMakeEmpty(_ target: CMTimeRange) -> CMTimeMapping
@available(tvOS 9.0, *)
@discardableResult
func CMTimeMappingCopyAsDictionary(_ mapping: CMTimeMapping, _ allocator: CFAllocator?) -> CFDictionary?
@available(tvOS 9.0, *)
@discardableResult
func CMTimeMappingMakeFromDictionary(_ dict: CFDictionary) -> CMTimeMapping
@available(tvOS 9.0, *)
let kCMTimeMappingSourceKey: CFString
@available(tvOS 9.0, *)
let kCMTimeMappingTargetKey: CFString
@available(tvOS 9.0, *)
@discardableResult
func CMTimeMappingCopyDescription(_ allocator: CFAllocator?, _ mapping: CMTimeMapping) -> CFString?
@available(tvOS 9.0, *)
func CMTimeMappingShow(_ mapping: CMTimeMapping)
