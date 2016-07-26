
@available(OSX 10.4, *)
class DOMMouseEvent : DOMUIEvent {
  var screenX: Int32 { get }
  var screenY: Int32 { get }
  var clientX: Int32 { get }
  var clientY: Int32 { get }
  var ctrlKey: Bool { get }
  var shiftKey: Bool { get }
  var altKey: Bool { get }
  var metaKey: Bool { get }
  var button: UInt16 { get }
  var relatedTarget: DOMEventTarget! { get }
  @available(OSX 10.5, *)
  var offsetX: Int32 { get }
  @available(OSX 10.5, *)
  var offsetY: Int32 { get }
  @available(OSX 10.5, *)
  var x: Int32 { get }
  @available(OSX 10.5, *)
  var y: Int32 { get }
  @available(OSX 10.5, *)
  var fromElement: DOMNode! { get }
  @available(OSX 10.5, *)
  var toElement: DOMNode! { get }
  @available(OSX 10.5, *)
  func initMouseEvent(_ type: String!, canBubble canBubble: Bool, cancelable cancelable: Bool, view view: DOMAbstractView!, detail detail: Int32, screenX screenX: Int32, screenY screenY: Int32, clientX clientX: Int32, clientY clientY: Int32, ctrlKey ctrlKey: Bool, altKey altKey: Bool, shiftKey shiftKey: Bool, metaKey metaKey: Bool, button button: UInt16, relatedTarget relatedTarget: DOMEventTarget!)
}
extension DOMMouseEvent {
}
