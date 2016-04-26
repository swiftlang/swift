
@available(OSX 10.9, *)
class GCControllerButtonInput : GCControllerElement {
  var valueChangedHandler: GCControllerButtonValueChangedHandler?
  @available(OSX 10.10, *)
  var pressedChangedHandler: GCControllerButtonValueChangedHandler?
  var value: Float { get }
  var isPressed: Bool { get }
}
typealias GCControllerButtonValueChangedHandler = (GCControllerButtonInput, Float, Bool) -> Void
