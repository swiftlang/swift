
var DOM_NONE: Int { get }
var DOM_CAPTURING_PHASE: Int { get }
var DOM_AT_TARGET: Int { get }
var DOM_BUBBLING_PHASE: Int { get }
@available(OSX 10.4, *)
class DOMEvent : DOMObject {
  var type: String! { get }
  var target: DOMEventTarget! { get }
  var currentTarget: DOMEventTarget! { get }
  var eventPhase: UInt16 { get }
  var bubbles: Bool { get }
  var cancelable: Bool { get }
  var timeStamp: DOMTimeStamp { get }
  @available(OSX 10.6, *)
  var srcElement: DOMEventTarget! { get }
  @available(OSX 10.6, *)
  var returnValue: Bool
  @available(OSX 10.6, *)
  var cancelBubble: Bool
  func stopPropagation()
  func preventDefault()
  @available(OSX 10.5, *)
  func initEvent(_ eventTypeArg: String!, canBubbleArg canBubbleArg: Bool, cancelableArg cancelableArg: Bool)
}
extension DOMEvent {
}
