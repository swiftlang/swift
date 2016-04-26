
@available(tvOS 7.0, *)
class GCControllerAxisInput : GCControllerElement {
  var valueChangedHandler: GCControllerAxisValueChangedHandler?
  var value: Float { get }
}
typealias GCControllerAxisValueChangedHandler = (GCControllerAxisInput, Float) -> Void
