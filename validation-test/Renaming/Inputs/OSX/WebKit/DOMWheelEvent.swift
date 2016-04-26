
var DOM_DOM_DELTA_PIXEL: Int { get }
var DOM_DOM_DELTA_LINE: Int { get }
var DOM_DOM_DELTA_PAGE: Int { get }
@available(OSX 10.5, *)
class DOMWheelEvent : DOMMouseEvent {
  @available(OSX 10.5, *)
  var wheelDeltaX: Int32 { get }
  @available(OSX 10.5, *)
  var wheelDeltaY: Int32 { get }
  var wheelDelta: Int32 { get }
  var isHorizontal: Bool { get }
  @available(OSX 10.5, *)
  func initWheelEvent(_ wheelDeltaX: Int32, wheelDeltaY wheelDeltaY: Int32, view view: DOMAbstractView!, screenX screenX: Int32, screenY screenY: Int32, clientX clientX: Int32, clientY clientY: Int32, ctrlKey ctrlKey: Bool, altKey altKey: Bool, shiftKey shiftKey: Bool, metaKey metaKey: Bool)
}
