
typealias CGWindowID = UInt32
enum CGWindowSharingType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case none
  case readOnly
  case readWrite
}
enum CGWindowBackingType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case backingStoreRetained
  case backingStoreNonretained
  case backingStoreBuffered
}
@available(OSX 10.5, *)
let kCGWindowNumber: CFString
@available(OSX 10.5, *)
let kCGWindowStoreType: CFString
@available(OSX 10.5, *)
let kCGWindowLayer: CFString
@available(OSX 10.5, *)
let kCGWindowBounds: CFString
@available(OSX 10.5, *)
let kCGWindowSharingState: CFString
@available(OSX 10.5, *)
let kCGWindowAlpha: CFString
@available(OSX 10.5, *)
let kCGWindowOwnerPID: CFString
@available(OSX 10.5, *)
let kCGWindowMemoryUsage: CFString
@available(OSX 10.5, *)
let kCGWindowOwnerName: CFString
@available(OSX 10.5, *)
let kCGWindowName: CFString
@available(OSX 10.5, *)
let kCGWindowIsOnscreen: CFString
@available(OSX 10.5, *)
let kCGWindowBackingLocationVideoMemory: CFString
struct CGWindowListOption : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var optionOnScreenOnly: CGWindowListOption { get }
  static var optionOnScreenAboveWindow: CGWindowListOption { get }
  static var optionOnScreenBelowWindow: CGWindowListOption { get }
  static var optionIncludingWindow: CGWindowListOption { get }
  static var excludeDesktopElements: CGWindowListOption { get }
}
@available(OSX 10.5, *)
@discardableResult
func CGWindowListCopyWindowInfo(_ option: CGWindowListOption, _ relativeToWindow: CGWindowID) -> CFArray?
@available(OSX 10.5, *)
@discardableResult
func CGWindowListCreateDescriptionFromArray(_ windowArray: CFArray?) -> CFArray?
struct CGWindowImageOption : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var boundsIgnoreFraming: CGWindowImageOption { get }
  static var shouldBeOpaque: CGWindowImageOption { get }
  static var onlyShadows: CGWindowImageOption { get }
  static var bestResolution: CGWindowImageOption { get }
  static var nominalResolution: CGWindowImageOption { get }
}
@available(OSX 10.5, *)
@discardableResult
func CGWindowListCreateImage(_ screenBounds: CGRect, _ listOption: CGWindowListOption, _ windowID: CGWindowID, _ imageOption: CGWindowImageOption) -> CGImage?
extension CGImage {
  @available(OSX 10.5, *)
  init?(windowListFromArrayScreenBounds screenBounds: CGRect, windowArray windowArray: CFArray, imageOption imageOption: CGWindowImageOption)
}
