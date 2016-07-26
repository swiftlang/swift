
var DOM_KEY_LOCATION_STANDARD: Int { get }
var DOM_KEY_LOCATION_LEFT: Int { get }
var DOM_KEY_LOCATION_RIGHT: Int { get }
var DOM_KEY_LOCATION_NUMPAD: Int { get }
@available(OSX 10.5, *)
class DOMKeyboardEvent : DOMUIEvent {
  var keyIdentifier: String! { get }
  @available(OSX 10.8, *)
  var location: UInt32 { get }
  var ctrlKey: Bool { get }
  var shiftKey: Bool { get }
  var altKey: Bool { get }
  var metaKey: Bool { get }
  @available(OSX 10.5, *)
  var altGraphKey: Bool { get }
  @discardableResult
  func getModifierState(_ keyIdentifierArg: String!) -> Bool
  @available(OSX 10.8, *)
  func initKeyboardEvent(_ type: String!, canBubble canBubble: Bool, cancelable cancelable: Bool, view view: DOMAbstractView!, keyIdentifier keyIdentifier: String!, location location: UInt32, ctrlKey ctrlKey: Bool, altKey altKey: Bool, shiftKey shiftKey: Bool, metaKey metaKey: Bool, altGraphKey altGraphKey: Bool)
  @available(OSX 10.8, *)
  func initKeyboardEvent(_ type: String!, canBubble canBubble: Bool, cancelable cancelable: Bool, view view: DOMAbstractView!, keyIdentifier keyIdentifier: String!, location location: UInt32, ctrlKey ctrlKey: Bool, altKey altKey: Bool, shiftKey shiftKey: Bool, metaKey metaKey: Bool)
}
