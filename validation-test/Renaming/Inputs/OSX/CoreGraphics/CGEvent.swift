
extension CGEvent {
  @available(OSX 10.4, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.4, *)
  init?(source source: CGEventSource?)
  @available(OSX 10.4, *)
  @discardableResult
  func createData(_ allocator: CFAllocator?) -> CFData?
  @available(OSX 10.4, *)
  /*not inherited*/ init?(withDataAllocator allocator: CFAllocator?, data data: CFData?)
  @available(OSX 10.4, *)
  init?(mouseEventSource source: CGEventSource?, mouseType mouseType: CGEventType, mouseCursorPosition mouseCursorPosition: CGPoint, mouseButton mouseButton: CGMouseButton)
  @available(OSX 10.4, *)
  init?(keyboardEventSource source: CGEventSource?, virtualKey virtualKey: CGKeyCode, keyDown keyDown: Bool)
  @available(OSX 10.4, *)
  init?(copy event: CGEvent?)
  @available(OSX 10.4, *)
  func setSource(_ source: CGEventSource?)
  @available(OSX 10.4, *)
  var type: CGEventType
  @available(OSX 10.4, *)
  var timestamp: CGEventTimestamp
  @available(OSX 10.4, *)
  var location: CGPoint
  @available(OSX 10.5, *)
  var unflippedLocation: CGPoint { get }
  @available(OSX 10.4, *)
  var flags: CGEventFlags
  @available(OSX 10.4, *)
  func keyboardGetUnicodeString(maxStringLength maxStringLength: Int, actualStringLength actualStringLength: UnsafeMutablePointer<Int>?, unicodeString unicodeString: UnsafeMutablePointer<UniChar>?)
  @available(OSX 10.4, *)
  func keyboardSetUnicodeString(stringLength stringLength: Int, unicodeString unicodeString: UnsafePointer<UniChar>?)
  @available(OSX 10.4, *)
  @discardableResult
  func getIntegerValueField(_ field: CGEventField) -> Int64
  @available(OSX 10.4, *)
  func setIntegerValueField(_ field: CGEventField, value value: Int64)
  @available(OSX 10.4, *)
  @discardableResult
  func getDoubleValueField(_ field: CGEventField) -> Double
  @available(OSX 10.4, *)
  func setDoubleValueField(_ field: CGEventField, value value: Double)
  @available(OSX 10.4, *)
  @discardableResult
  class func tapCreate(tap tap: CGEventTapLocation, place place: CGEventTapPlacement, options options: CGEventTapOptions, eventsOfInterest eventsOfInterest: CGEventMask, callback callback: CGEventTapCallBack?, userInfo userInfo: UnsafeMutablePointer<Void>?) -> CFMachPort?
  @available(OSX 10.4, *)
  @discardableResult
  class func tapCreateForPSN(processSerialNumber processSerialNumber: UnsafeMutablePointer<Void>, place place: CGEventTapPlacement, options options: CGEventTapOptions, eventsOfInterest eventsOfInterest: CGEventMask, callback callback: CGEventTapCallBack?, userInfo userInfo: UnsafeMutablePointer<Void>?) -> CFMachPort?
  @available(OSX 10.4, *)
  @discardableResult
  class func tapCreateForPid(pid pid: pid_t, place place: CGEventTapPlacement, options options: CGEventTapOptions, eventsOfInterest eventsOfInterest: CGEventMask, callback callback: CGEventTapCallBack, userInfo userInfo: UnsafeMutablePointer<Void>) -> CFMachPort?
  @available(OSX 10.4, *)
  class func tapEnable(tap tap: CFMachPort, enable enable: Bool)
  @available(OSX 10.4, *)
  @discardableResult
  class func tapIsEnabled(tap tap: CFMachPort) -> Bool
  @available(OSX 10.4, *)
  func tapPostEvent(_ proxy: CGEventTapProxy?)
  @available(OSX 10.4, *)
  func post(tap tap: CGEventTapLocation)
  @available(OSX 10.4, *)
  func postToPSN(processSerialNumber processSerialNumber: UnsafeMutablePointer<Void>?)
  @available(OSX 10.11, *)
  func postToPid(_ pid: pid_t)
}
extension CGEventSource {
  @available(OSX 10.4, *)
  /*not inherited*/ init?(_ event: CGEvent?)
}
@available(OSX 10.4, *)
@discardableResult
func CGGetEventTapList(_ maxNumberOfTaps: UInt32, _ tapList: UnsafeMutablePointer<CGEventTapInformation>?, _ eventTapCount: UnsafeMutablePointer<UInt32>?) -> CGError
