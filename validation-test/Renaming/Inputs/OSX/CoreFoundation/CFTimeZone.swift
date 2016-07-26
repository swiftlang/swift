
@discardableResult
func CFTimeZoneGetTypeID() -> CFTypeID
@discardableResult
func CFTimeZoneCopySystem() -> CFTimeZone!
func CFTimeZoneResetSystem()
@discardableResult
func CFTimeZoneCopyDefault() -> CFTimeZone!
func CFTimeZoneSetDefault(_ tz: CFTimeZone!)
@discardableResult
func CFTimeZoneCopyKnownNames() -> CFArray!
@discardableResult
func CFTimeZoneCopyAbbreviationDictionary() -> CFDictionary!
func CFTimeZoneSetAbbreviationDictionary(_ dict: CFDictionary!)
@discardableResult
func CFTimeZoneCreate(_ allocator: CFAllocator!, _ name: CFString!, _ data: CFData!) -> CFTimeZone!
@discardableResult
func CFTimeZoneCreateWithTimeIntervalFromGMT(_ allocator: CFAllocator!, _ ti: CFTimeInterval) -> CFTimeZone!
@discardableResult
func CFTimeZoneCreateWithName(_ allocator: CFAllocator!, _ name: CFString!, _ tryAbbrev: Bool) -> CFTimeZone!
@discardableResult
func CFTimeZoneGetName(_ tz: CFTimeZone!) -> CFString!
@discardableResult
func CFTimeZoneGetData(_ tz: CFTimeZone!) -> CFData!
@discardableResult
func CFTimeZoneGetSecondsFromGMT(_ tz: CFTimeZone!, _ at: CFAbsoluteTime) -> CFTimeInterval
@discardableResult
func CFTimeZoneCopyAbbreviation(_ tz: CFTimeZone!, _ at: CFAbsoluteTime) -> CFString!
@discardableResult
func CFTimeZoneIsDaylightSavingTime(_ tz: CFTimeZone!, _ at: CFAbsoluteTime) -> Bool
@available(OSX 10.5, *)
@discardableResult
func CFTimeZoneGetDaylightSavingTimeOffset(_ tz: CFTimeZone!, _ at: CFAbsoluteTime) -> CFTimeInterval
@available(OSX 10.5, *)
@discardableResult
func CFTimeZoneGetNextDaylightSavingTimeTransition(_ tz: CFTimeZone!, _ at: CFAbsoluteTime) -> CFAbsoluteTime
@available(OSX 10.5, *)
enum CFTimeZoneNameStyle : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case standard
  case shortStandard
  case daylightSaving
  case shortDaylightSaving
  case generic
  case shortGeneric
}
@available(OSX 10.5, *)
@discardableResult
func CFTimeZoneCopyLocalizedName(_ tz: CFTimeZone!, _ style: CFTimeZoneNameStyle, _ locale: CFLocale!) -> CFString!
@available(OSX 10.5, *)
let kCFTimeZoneSystemTimeZoneDidChangeNotification: CFString!
