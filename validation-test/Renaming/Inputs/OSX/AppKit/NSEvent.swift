
enum NSEventType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case leftMouseDown
  case leftMouseUp
  case rightMouseDown
  case rightMouseUp
  case mouseMoved
  case leftMouseDragged
  case rightMouseDragged
  case mouseEntered
  case mouseExited
  case keyDown
  case keyUp
  case flagsChanged
  case appKitDefined
  case systemDefined
  case applicationDefined
  case periodic
  case cursorUpdate
  case scrollWheel
  case tabletPoint
  case tabletProximity
  case otherMouseDown
  case otherMouseUp
  case otherMouseDragged
  @available(OSX 10.5, *)
  case eventTypeGesture
  @available(OSX 10.5, *)
  case eventTypeMagnify
  @available(OSX 10.5, *)
  case eventTypeSwipe
  @available(OSX 10.5, *)
  case eventTypeRotate
  @available(OSX 10.5, *)
  case eventTypeBeginGesture
  @available(OSX 10.5, *)
  case eventTypeEndGesture
  @available(OSX 10.8, *)
  case eventTypeSmartMagnify
  @available(OSX 10.8, *)
  case eventTypeQuickLook
  @available(OSX 10.10.3, *)
  case eventTypePressure
}
struct NSEventMask : OptionSet {
  init(rawValue rawValue: UInt64)
  let rawValue: UInt64
  static var leftMouseDownMask: NSEventMask { get }
  static var leftMouseUpMask: NSEventMask { get }
  static var rightMouseDownMask: NSEventMask { get }
  static var rightMouseUpMask: NSEventMask { get }
  static var mouseMovedMask: NSEventMask { get }
  static var leftMouseDraggedMask: NSEventMask { get }
  static var rightMouseDraggedMask: NSEventMask { get }
  static var mouseEnteredMask: NSEventMask { get }
  static var mouseExitedMask: NSEventMask { get }
  static var keyDownMask: NSEventMask { get }
  static var keyUpMask: NSEventMask { get }
  static var flagsChangedMask: NSEventMask { get }
  static var appKitDefinedMask: NSEventMask { get }
  static var systemDefinedMask: NSEventMask { get }
  static var applicationDefinedMask: NSEventMask { get }
  static var periodicMask: NSEventMask { get }
  static var cursorUpdateMask: NSEventMask { get }
  static var scrollWheelMask: NSEventMask { get }
  static var tabletPointMask: NSEventMask { get }
  static var tabletProximityMask: NSEventMask { get }
  static var otherMouseDownMask: NSEventMask { get }
  static var otherMouseUpMask: NSEventMask { get }
  static var otherMouseDraggedMask: NSEventMask { get }
  @available(OSX 10.5, *)
  static var eventMaskGesture: NSEventMask { get }
  @available(OSX 10.5, *)
  static var eventMaskMagnify: NSEventMask { get }
  @available(OSX 10.5, *)
  static var eventMaskSwipe: NSEventMask { get }
  @available(OSX 10.5, *)
  static var eventMaskRotate: NSEventMask { get }
  @available(OSX 10.5, *)
  static var eventMaskBeginGesture: NSEventMask { get }
  @available(OSX 10.5, *)
  static var eventMaskEndGesture: NSEventMask { get }
  @available(OSX 10.8, *)
  static var eventMaskSmartMagnify: NSEventMask { get }
  @available(OSX 10.10.3, *)
  static var eventMaskPressure: NSEventMask { get }
  static var anyEventMask: NSEventMask { get }
}
@discardableResult
func NSEventMaskFromType(_ type: NSEventType) -> NSEventMask
struct NSEventModifierFlags : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var alphaShiftKeyMask: NSEventModifierFlags { get }
  static var shiftKeyMask: NSEventModifierFlags { get }
  static var controlKeyMask: NSEventModifierFlags { get }
  static var alternateKeyMask: NSEventModifierFlags { get }
  static var commandKeyMask: NSEventModifierFlags { get }
  static var numericPadKeyMask: NSEventModifierFlags { get }
  static var helpKeyMask: NSEventModifierFlags { get }
  static var functionKeyMask: NSEventModifierFlags { get }
  static var deviceIndependentModifierFlagsMask: NSEventModifierFlags { get }
}
enum NSPointingDeviceType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case unknownPointingDevice
  case penPointingDevice
  case cursorPointingDevice
  case eraserPointingDevice
}
struct NSEventButtonMask : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var penTipMask: NSEventButtonMask { get }
  static var penLowerSideMask: NSEventButtonMask { get }
  static var penUpperSideMask: NSEventButtonMask { get }
}
@available(OSX 10.7, *)
struct NSEventPhase : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var began: NSEventPhase { get }
  static var stationary: NSEventPhase { get }
  static var changed: NSEventPhase { get }
  static var ended: NSEventPhase { get }
  static var cancelled: NSEventPhase { get }
  static var mayBegin: NSEventPhase { get }
}
@available(OSX 10.7, *)
enum NSEventGestureAxis : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case horizontal
  case vertical
}
@available(OSX 10.7, *)
struct NSEventSwipeTrackingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var lockDirection: NSEventSwipeTrackingOptions { get }
  static var clampGestureAmount: NSEventSwipeTrackingOptions { get }
}
enum NSEventSubtype : Int16 {
  init?(rawValue rawValue: Int16)
  var rawValue: Int16 { get }
  case NSWindowExposedEventType
  case NSApplicationActivatedEventType
  case NSApplicationDeactivatedEventType
  case NSWindowMovedEventType
  case NSScreenChangedEventType
  case NSAWTEventType
  static var NSPowerOffEventType: NSEventSubtype { get }
  static var NSMouseEventSubtype: NSEventSubtype { get }
  static var NSTabletPointEventSubtype: NSEventSubtype { get }
  static var NSTabletProximityEventSubtype: NSEventSubtype { get }
  @available(OSX 10.6, *)
  case NSTouchEventSubtype
}
@available(OSX 10.10.3, *)
enum NSPressureBehavior : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case primaryDefault
  case primaryClick
  case primaryGeneric
  case primaryAccelerator
  case primaryDeepClick
  case primaryDeepDrag
}
class NSEvent : NSObject, NSCopying, NSCoding {
  var type: NSEventType { get }
  var modifierFlags: NSEventModifierFlags { get }
  var timestamp: NSTimeInterval { get }
  unowned(unsafe) var window: @sil_unmanaged NSWindow? { get }
  var windowNumber: Int { get }
  var context: NSGraphicsContext? { get }
  var clickCount: Int { get }
  var buttonNumber: Int { get }
  var eventNumber: Int { get }
  var pressure: Float { get }
  var locationInWindow: NSPoint { get }
  var deltaX: CGFloat { get }
  var deltaY: CGFloat { get }
  var deltaZ: CGFloat { get }
  @available(OSX 10.7, *)
  var hasPreciseScrollingDeltas: Bool { get }
  @available(OSX 10.7, *)
  var scrollingDeltaX: CGFloat { get }
  @available(OSX 10.7, *)
  var scrollingDeltaY: CGFloat { get }
  @available(OSX 10.7, *)
  var momentumPhase: NSEventPhase { get }
  @available(OSX 10.7, *)
  var isDirectionInvertedFromDevice: Bool { get }
  var characters: String? { get }
  var charactersIgnoringModifiers: String? { get }
  var isARepeat: Bool { get }
  var keyCode: UInt16 { get }
  var trackingNumber: Int { get }
  var userData: UnsafeMutablePointer<Void>? { get }
  @available(OSX 10.5, *)
  var trackingArea: NSTrackingArea? { get }
  var subtype: NSEventSubtype { get }
  var data1: Int { get }
  var data2: Int { get }
  @available(OSX 10.5, *)
  var eventRef: UnsafePointer<Void>? { get }
  @available(OSX 10.5, *)
  /*not inherited*/ init?(eventRef eventRef: UnsafePointer<Void>)
  @available(OSX 10.5, *)
  var cgEvent: CGEvent? { get }
  @available(OSX 10.5, *)
  /*not inherited*/ init?(cgEvent cgEvent: CGEvent)
  @available(OSX 10.5, *)
  class func setMouseCoalescingEnabled(_ flag: Bool)
  @available(OSX 10.5, *)
  @discardableResult
  class func isMouseCoalescingEnabled() -> Bool
  @available(OSX 10.5, *)
  var magnification: CGFloat { get }
  var deviceID: Int { get }
  var rotation: Float { get }
  var absoluteX: Int { get }
  var absoluteY: Int { get }
  var absoluteZ: Int { get }
  var buttonMask: NSEventButtonMask { get }
  var tilt: NSPoint { get }
  var tangentialPressure: Float { get }
  var vendorDefined: AnyObject { get }
  var vendorID: Int { get }
  var tabletID: Int { get }
  var pointingDeviceID: Int { get }
  var systemTabletID: Int { get }
  var vendorPointingDeviceType: Int { get }
  var pointingDeviceSerialNumber: Int { get }
  var uniqueID: UInt64 { get }
  var capabilityMask: Int { get }
  var pointingDeviceType: NSPointingDeviceType { get }
  var isEnteringProximity: Bool { get }
  @available(OSX 10.6, *)
  @discardableResult
  func touches(matching phase: NSTouchPhase, in view: NSView?) -> Set<NSTouch>
  @available(OSX 10.7, *)
  var phase: NSEventPhase { get }
  @available(OSX 10.10.3, *)
  var stage: Int { get }
  @available(OSX 10.10.3, *)
  var stageTransition: CGFloat { get }
  @available(OSX 10.10.3, *)
  var associatedEventsMask: NSEventMask { get }
  @available(OSX 10.11, *)
  var pressureBehavior: NSPressureBehavior { get }
  @available(OSX 10.7, *)
  @discardableResult
  class func isSwipeTrackingFromScrollEventsEnabled() -> Bool
  @available(OSX 10.7, *)
  func trackSwipeEvent(_ options: NSEventSwipeTrackingOptions = [], dampenAmountThresholdMin minDampenThreshold: CGFloat, max maxDampenThreshold: CGFloat, usingHandler trackingHandler: (CGFloat, NSEventPhase, Bool, UnsafeMutablePointer<ObjCBool>) -> Void)
  class func startPeriodicEvents(afterDelay delay: NSTimeInterval, withPeriod period: NSTimeInterval)
  class func stopPeriodicEvents()
  @discardableResult
  class func mouseEvent(with type: NSEventType, location location: NSPoint, modifierFlags flags: NSEventModifierFlags, timestamp time: NSTimeInterval, windowNumber wNum: Int, context context: NSGraphicsContext?, eventNumber eNum: Int, clickCount cNum: Int, pressure pressure: Float) -> NSEvent?
  @discardableResult
  class func keyEvent(with type: NSEventType, location location: NSPoint, modifierFlags flags: NSEventModifierFlags, timestamp time: NSTimeInterval, windowNumber wNum: Int, context context: NSGraphicsContext?, characters keys: String, charactersIgnoringModifiers ukeys: String, isARepeat flag: Bool, keyCode code: UInt16) -> NSEvent?
  @discardableResult
  class func enterExitEvent(with type: NSEventType, location location: NSPoint, modifierFlags flags: NSEventModifierFlags, timestamp time: NSTimeInterval, windowNumber wNum: Int, context context: NSGraphicsContext?, eventNumber eNum: Int, trackingNumber tNum: Int, userData data: UnsafeMutablePointer<Void>?) -> NSEvent?
  @discardableResult
  class func otherEvent(with type: NSEventType, location location: NSPoint, modifierFlags flags: NSEventModifierFlags, timestamp time: NSTimeInterval, windowNumber wNum: Int, context context: NSGraphicsContext?, subtype subtype: Int16, data1 d1: Int, data2 d2: Int) -> NSEvent?
  @discardableResult
  class func mouseLocation() -> NSPoint
  @available(OSX 10.6, *)
  @discardableResult
  class func modifierFlags() -> NSEventModifierFlags
  @available(OSX 10.6, *)
  @discardableResult
  class func pressedMouseButtons() -> Int
  @available(OSX 10.6, *)
  @discardableResult
  class func doubleClickInterval() -> NSTimeInterval
  @available(OSX 10.6, *)
  @discardableResult
  class func keyRepeatDelay() -> NSTimeInterval
  @available(OSX 10.6, *)
  @discardableResult
  class func keyRepeatInterval() -> NSTimeInterval
  @available(OSX 10.6, *)
  @discardableResult
  class func addGlobalMonitorForEvents(matching mask: NSEventMask, handler block: (NSEvent) -> Void) -> AnyObject?
  @available(OSX 10.6, *)
  @discardableResult
  class func addLocalMonitorForEvents(matching mask: NSEventMask, handler block: (NSEvent) -> NSEvent?) -> AnyObject?
  @available(OSX 10.6, *)
  class func removeMonitor(_ eventMonitor: AnyObject)
}
var NSUpArrowFunctionKey: Int { get }
var NSDownArrowFunctionKey: Int { get }
var NSLeftArrowFunctionKey: Int { get }
var NSRightArrowFunctionKey: Int { get }
var NSF1FunctionKey: Int { get }
var NSF2FunctionKey: Int { get }
var NSF3FunctionKey: Int { get }
var NSF4FunctionKey: Int { get }
var NSF5FunctionKey: Int { get }
var NSF6FunctionKey: Int { get }
var NSF7FunctionKey: Int { get }
var NSF8FunctionKey: Int { get }
var NSF9FunctionKey: Int { get }
var NSF10FunctionKey: Int { get }
var NSF11FunctionKey: Int { get }
var NSF12FunctionKey: Int { get }
var NSF13FunctionKey: Int { get }
var NSF14FunctionKey: Int { get }
var NSF15FunctionKey: Int { get }
var NSF16FunctionKey: Int { get }
var NSF17FunctionKey: Int { get }
var NSF18FunctionKey: Int { get }
var NSF19FunctionKey: Int { get }
var NSF20FunctionKey: Int { get }
var NSF21FunctionKey: Int { get }
var NSF22FunctionKey: Int { get }
var NSF23FunctionKey: Int { get }
var NSF24FunctionKey: Int { get }
var NSF25FunctionKey: Int { get }
var NSF26FunctionKey: Int { get }
var NSF27FunctionKey: Int { get }
var NSF28FunctionKey: Int { get }
var NSF29FunctionKey: Int { get }
var NSF30FunctionKey: Int { get }
var NSF31FunctionKey: Int { get }
var NSF32FunctionKey: Int { get }
var NSF33FunctionKey: Int { get }
var NSF34FunctionKey: Int { get }
var NSF35FunctionKey: Int { get }
var NSInsertFunctionKey: Int { get }
var NSDeleteFunctionKey: Int { get }
var NSHomeFunctionKey: Int { get }
var NSBeginFunctionKey: Int { get }
var NSEndFunctionKey: Int { get }
var NSPageUpFunctionKey: Int { get }
var NSPageDownFunctionKey: Int { get }
var NSPrintScreenFunctionKey: Int { get }
var NSScrollLockFunctionKey: Int { get }
var NSPauseFunctionKey: Int { get }
var NSSysReqFunctionKey: Int { get }
var NSBreakFunctionKey: Int { get }
var NSResetFunctionKey: Int { get }
var NSStopFunctionKey: Int { get }
var NSMenuFunctionKey: Int { get }
var NSUserFunctionKey: Int { get }
var NSSystemFunctionKey: Int { get }
var NSPrintFunctionKey: Int { get }
var NSClearLineFunctionKey: Int { get }
var NSClearDisplayFunctionKey: Int { get }
var NSInsertLineFunctionKey: Int { get }
var NSDeleteLineFunctionKey: Int { get }
var NSInsertCharFunctionKey: Int { get }
var NSDeleteCharFunctionKey: Int { get }
var NSPrevFunctionKey: Int { get }
var NSNextFunctionKey: Int { get }
var NSSelectFunctionKey: Int { get }
var NSExecuteFunctionKey: Int { get }
var NSUndoFunctionKey: Int { get }
var NSRedoFunctionKey: Int { get }
var NSFindFunctionKey: Int { get }
var NSHelpFunctionKey: Int { get }
var NSModeSwitchFunctionKey: Int { get }
