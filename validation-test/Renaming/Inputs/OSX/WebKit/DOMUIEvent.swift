
@available(OSX 10.4, *)
class DOMUIEvent : DOMEvent {
  var view: DOMAbstractView! { get }
  var detail: Int32 { get }
  @available(OSX 10.5, *)
  var keyCode: Int32 { get }
  @available(OSX 10.5, *)
  var charCode: Int32 { get }
  @available(OSX 10.5, *)
  var pageX: Int32 { get }
  @available(OSX 10.5, *)
  var pageY: Int32 { get }
  @available(OSX 10.5, *)
  var which: Int32 { get }
  @available(OSX 10.5, *)
  func initUIEvent(_ type: String!, canBubble canBubble: Bool, cancelable cancelable: Bool, view view: DOMAbstractView!, detail detail: Int32)
}
extension DOMUIEvent {
}
