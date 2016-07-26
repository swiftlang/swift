
@available(OSX 10.9, *)
class GCControllerDirectionPad : GCControllerElement {
  var valueChangedHandler: GCControllerDirectionPadValueChangedHandler?
  var xAxis: GCControllerAxisInput { get }
  var yAxis: GCControllerAxisInput { get }
  var up: GCControllerButtonInput { get }
  var down: GCControllerButtonInput { get }
  var left: GCControllerButtonInput { get }
  var right: GCControllerButtonInput { get }
}
typealias GCControllerDirectionPadValueChangedHandler = (GCControllerDirectionPad, Float, Float) -> Void
