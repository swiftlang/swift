
extension CGEventSource {
  @available(OSX 10.4, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.4, *)
  init?(stateID stateID: CGEventSourceStateID)
  @available(OSX 10.4, *)
  var keyboardType: CGEventSourceKeyboardType
  @available(OSX 10.5, *)
  var pixelsPerLine: Double
  @available(OSX 10.4, *)
  var sourceStateID: CGEventSourceStateID { get }
  @available(OSX 10.4, *)
  @discardableResult
  class func buttonState(_ stateID: CGEventSourceStateID, button button: CGMouseButton) -> Bool
  @available(OSX 10.4, *)
  @discardableResult
  class func keyState(_ stateID: CGEventSourceStateID, key key: CGKeyCode) -> Bool
  @available(OSX 10.4, *)
  @discardableResult
  class func flagsState(_ stateID: CGEventSourceStateID) -> CGEventFlags
  @available(OSX 10.4, *)
  @discardableResult
  class func secondsSinceLastEventType(_ stateID: CGEventSourceStateID, eventType eventType: CGEventType) -> CFTimeInterval
  @available(OSX 10.4, *)
  @discardableResult
  class func counterForEventType(_ stateID: CGEventSourceStateID, eventType eventType: CGEventType) -> UInt32
  var userData: Int64
  @available(OSX 10.4, *)
  func setLocalEventsFilterDuringSuppressionState(_ filter: CGEventFilterMask, state state: CGEventSuppressionState)
  @available(OSX 10.4, *)
  @discardableResult
  func getLocalEventsFilterDuringSuppressionState(_ state: CGEventSuppressionState) -> CGEventFilterMask
  var localEventsSuppressionInterval: CFTimeInterval
}
