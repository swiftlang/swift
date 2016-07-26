
@available(iOS 7.0, *)
class GCControllerButtonInput : GCControllerElement {
  var valueChangedHandler: GCControllerButtonValueChangedHandler?
  @available(iOS 8.0, *)
  var pressedChangedHandler: GCControllerButtonValueChangedHandler?
  var value: Float { get }
  var isPressed: Bool { get }
}
typealias GCControllerButtonValueChangedHandler = (GCControllerButtonInput, Float, Bool) -> Void
